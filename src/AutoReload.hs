{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AutoReload
  ( notifyClientsFor,
    addClient,
    removeClient,
    ClientConnection,
    connId,
    wsConn,
  )
where

import Colog.Message
import Colog.Monad
import Config (Route)
import Control.Concurrent.MVar
import Control.Exception (catch)
import Control.Monad (forM_)
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Network.WebSockets (Connection, ConnectionException, sendTextData)
import System.IO.Unsafe (unsafePerformIO)

data ClientConnection = ClientConnection
  { connId :: Int,
    wsConn :: Connection
  }

connectionCounter :: MVar Int
connectionCounter = unsafePerformIO $ newMVar 0

activeConnections :: MVar [ClientConnection]
activeConnections = unsafePerformIO $ newMVar []

getNextConnectionId :: IO Int
getNextConnectionId = modifyMVar connectionCounter $ \i -> return (i + 1, i)

addClient :: Connection -> IO ClientConnection
addClient conn = do
  connId' <- getNextConnectionId
  let clientConn = ClientConnection connId' conn
  modifyMVar_ activeConnections $ \conns -> return (clientConn : conns)
  return clientConn

removeClient :: Int -> IO ()
removeClient cid = modifyMVar_ activeConnections $ \conns ->
  return (filter (\c -> connId c /= cid) conns)

notifyClientsFor :: (WithLog env Message m, MonadIO m) => Route -> Text -> m ()
notifyClientsFor route message =
  notifyClients (message <> ":" <> route)

notifyClients :: (WithLog env Message m, MonadIO m) => Text -> m ()
notifyClients message = do
  clients <- liftIO $ readMVar activeConnections

  forM_ clients $ \client -> do
    liftIO $
      catch
        (sendTextData (wsConn client) message)
        ( \(_ :: ConnectionException) -> do
            removeClient (connId client)
        )

  logInfo $ "Notified " <> T.pack (show (length clients)) <> " clients: " <> message

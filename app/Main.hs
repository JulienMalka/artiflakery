{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Auth
  ( checkBasicAuth,
    isAuthorized,
    makeSecureCookieHeader,
    parseAuthFile,
    signCookieValue,
    getSecretKey
  )
import AutoReload (addClient, connId, removeClient)
import BuildFlake (buildFlakeWithLogging)
import Colog.Message
import Colog.Monad
import Config (FlakeRef, Group, Route, RouteMap, UserDB, parseConfigFile)
import Control.Concurrent.Async
import Control.Exception (finally)
import Control.Monad (forever, void)
import Control.Monad.IO.Class
import Data.List (maximumBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Static (defaultFileServerSettings, ssAddTrailingSlash, staticApp)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsApp)
import Network.WebSockets hiding (Message)
import Server
import System.Directory
import System.FilePath (dropTrailingPathSeparator, takeDirectory)
import Logger
import Options.Applicative

findMatchingRoute :: Text -> RouteMap -> Maybe (Route, (FlakeRef, [Group]))
findMatchingRoute path routeMap =
  let normalized = T.dropWhile (== '/') path <> "/"
      isMatch (r, _) = r `T.isPrefixOf` normalized
      matches = filter isMatch (Map.toList routeMap)
   in case filter isMatch (Map.toList routeMap) of
        [] -> Nothing
        _ -> Just $ maximumBy (comparing (T.length . fst)) matches

app :: 
  RouteMap ->
  UserDB ->
  LoggedApplication env m
app routeMap authDB req respond = do
  secret <- liftIO $ getSecretKey
  let rawPath = decodeUtf8 $ rawPathInfo req
      normalizedPath = T.dropWhile (== '/') rawPath

  logDebug $ "Incoming request path: " <> normalizedPath

  case findMatchingRoute normalizedPath routeMap of
    Nothing -> do
      logWarning $ "No matching auth route. Serving static: " <> normalizedPath
      liftIO $ staticApp (defaultFileServerSettings "data") {ssAddTrailingSlash = True} req respond
    Just (matchedRoute, (_, allowedGroups)) -> do
      if normalizedPath /= matchedRoute && (normalizedPath <> "/") == matchedRoute
        then liftIO $ respond $ responseBuilder status301 [("Location", encodeUtf8 ("/" <> matchedRoute))] mempty
        else do
          (authorized, groups) <- isAuthorized allowedGroups req
          if authorized
            then serve normalizedPath routeMap authDB req respond
            else do
              result <- liftIO $ checkBasicAuth authDB allowedGroups req
              case result of
                Just group -> do
                  let groupStr = signCookieValue secret (encodeUtf8 (T.intercalate "," (group : groups)))
                  cookieHeader <- liftIO $ makeSecureCookieHeader (decodeUtf8 groupStr)
                  serveWithCookie normalizedPath cookieHeader routeMap authDB req respond
                Nothing ->
                  liftIO $
                    respond $
                      responseLBS
                        status401
                        [("WWW-Authenticate", "Basic realm=\"Access to " <> encodeUtf8 matchedRoute <> "\"")]
                        "Unauthorized"

webSocketHandler :: (WithLog env Message m, MonadIO m) => PendingConnection -> m ()
webSocketHandler pendingConn = do
  logInfo "New WebSocket connection accepted."
  conn <- liftIO $ acceptRequest pendingConn
  client <- liftIO $ addClient conn
  _ <-
    liftIO $
      finally
        (forever $ void (receiveData conn :: IO Text))
        (pure ()) 
  logInfo $ "Closing connection: " <> T.pack (show (connId client))
  liftIO $ removeClient (connId client)

createDataSkeleton :: (WithLog env Message m, MonadIO m) => RouteMap -> m ()
createDataSkeleton routeMap = do
  logInfo "Creating data skeleton..."
  liftIO $ removeDirectoryRecursive "data"
  let createR r = createDirectoryIfMissing True ("data/" ++ takeDirectory (dropTrailingPathSeparator (T.unpack r)))
  liftIO $ mapM_ createR (Map.keys routeMap)

appWithLogging :: forall env m. RouteMap -> UserDB -> LoggedApplication env m
appWithLogging routeMap authDB req respond =
  do
    case websocketsApp defaultConnectionOptions (usingLoggerT coloredLogAction . webSocketHandler) req of
      Just wsResp -> liftIO $ respond wsResp
      Nothing -> app routeMap authDB req respond



data AppOptions = AppOptions
  { routesFile :: FilePath
  , authFile   :: FilePath
  }


parseOptions :: Parser AppOptions
parseOptions = AppOptions
  <$> strOption
      ( long "routes"
      <> metavar "FILE"
      <> help "Routes configuration file"
      <> showDefault )
  <*> strOption
      ( long "auth"
      <> metavar "FILE"
      <> help "Authentication database file"
      <> showDefault )


main :: IO ()
main = do
  options <- execParser opts
  routeMap <- parseConfigFile (routesFile options)
  authDB <- parseAuthFile (authFile options)

  usingLoggerT coloredLogAction $ createDataSkeleton routeMap

  _ <- async $ usingLoggerT coloredLogAction $ mapM_ (uncurry buildFlakeWithLogging) (Map.toList $ Map.map fst routeMap)

  let loggedApp = appWithLogging routeMap authDB 
  let app' req respond = usingLoggerT coloredLogAction (loggedApp req respond)

  run 8080 app'
  where
    opts = info (parseOptions <**> helper)
      ( fullDesc
      <> progDesc "Run the web application with custom routes and auth files"
      <> header "Web App - A configurable web application" )


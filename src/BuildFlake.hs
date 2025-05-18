{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module BuildFlake (buildFlakeWithLogging) where

import AutoReload (notifyClientsFor)
import Colog.Message
import Colog.Monad
import Config (FlakeRef, Route)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad.Catch (bracket)
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Logger
import System.Environment (getEnvironment)
import System.FilePath (dropTrailingPathSeparator)
import System.IO (BufferMode (LineBuffering), Handle, hGetLine, hIsEOF, hSetBuffering)
import System.IO.Error (catchIOError)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Files (readSymbolicLink)
import System.Process
import Text.Printf (printf)

type RouteLocks = IORef (Map.Map Route (MVar ()))

{-# NOINLINE routeLocks #-}
routeLocks :: RouteLocks
routeLocks = unsafePerformIO $ newIORef Map.empty

streamStderr :: (WithLog env Message m, MonadIO m) => Handle -> MVar () -> m ()
streamStderr h done = do
  let loop = do
        eof <- liftIO $ hIsEOF h
        if eof
          then liftIO $ putMVar done ()
          else do
            line <- liftIO $ hGetLine h
            logDebug $ T.pack line
            loop
  loop

buildFlakeWithLogging :: (WithLog env Message m, MonadIO m) => Route -> FlakeRef -> m ()
buildFlakeWithLogging route flakeref = do
  locks <- liftIO $ readIORef routeLocks
  lock <- case Map.lookup route locks of
    Just l -> return l
    Nothing -> do
      newLock <- liftIO $ newMVar ()
      liftIO $ atomicModifyIORef' routeLocks $ \m -> (Map.insert route newLock m, newLock)

  acquired <- liftIO $ tryTakeMVar lock
  let runBuildIO route' ref = usingLoggerT coloredLogAction (runBuild route' ref)
  case acquired of
    Nothing -> logInfo $ "Build already in progress for route: " <> route
    Just () -> do
      liftIO $
        bracket
          (return ())
          (\_ -> liftIO $ putMVar lock ())
          (\_ -> runBuildIO route flakeref)

runBuild :: (WithLog env Message m, MonadIO m) => Route -> FlakeRef -> m ()
runBuild route flakeref = do
  let output = "data/" <> route
  logInfo $ "Starting build for " <> flakeref
  currentEnv <- liftIO getEnvironment

  oldTarget <-
    liftIO $
      catchIOError
        (readSymbolicLink $ dropTrailingPathSeparator (T.unpack output))
        (\_ -> return "")

  let processConfig =
        (shell $ "nix build --refresh -o " <> T.unpack output <> " " <> T.unpack flakeref)
          { std_err = CreatePipe,
            env = Just currentEnv
          }

  result <- liftIO $ createProcess processConfig
  case result of
    (_, _, Just herr, ph) -> do
      liftIO $ hSetBuffering herr LineBuffering
      stderrDone <- liftIO newEmptyMVar

      _ <- liftIO $ forkIO $ usingLoggerT coloredLogAction (streamStderr herr stderrDone)

      exitCode <- liftIO $ waitForProcess ph
      _ <- liftIO $ takeMVar stderrDone

      newTarget <-
        liftIO $
          catchIOError
            (readSymbolicLink $ dropTrailingPathSeparator (T.unpack output))
            (\_ -> return "")

      logInfo $
        T.pack $
          printf
            "Completed building %s with exit code %s"
            (T.unpack flakeref)
            (show exitCode)

      if oldTarget /= newTarget && not (null newTarget)
        then do
          logInfo "Build produced new result, notifying clients"
          notifyClientsFor route "Build Complete!"
        else logInfo "No changes in build result, not notifying clients"
    _ -> logError "Failed to create process with expected stderr handle"

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module Server
  ( serve,
    serveWithCookie,
    serveFlakeWithCookie,
    serveStaticWithCookie,
    serveFlakePath,
    LoggedApplication,
  )
where

import BuildFlake (buildFlakeWithLogging)
import Colog.Message
import Colog.Monad
import Config
import Control.Monad.IO.Class
import Control.Concurrent.Async
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Static (defaultFileServerSettings, ssAddTrailingSlash, staticApp)
import System.Directory
import System.FilePath ((</>))
import Logger

type LoggedApplication env m =
  (WithLog env Message m, MonadIO m) =>
  Network.Wai.Request ->
  (Network.Wai.Response -> IO ResponseReceived) ->
  m ResponseReceived

websocketScriptFor :: Route -> Text
websocketScriptFor route =
  T.unlines
    [ "<script>",
      "  const socket = new WebSocket('ws://' + window.location.host + '/ws');",
      "  socket.addEventListener('message', function(event) {",
      "    if (event.data === 'Build Complete!:" <> route <> "') {",
      "      window.location.reload();",
      "    }",
      "  });",
      "</script>"
    ]

notFound :: (Network.Wai.Response -> IO ResponseReceived) -> IO ResponseReceived
notFound respond =
  respond $ responseLBS status404 [("Content-Type", "text/plain")] "404 - Not Found"

serveFlakePath :: Route -> FlakeRef -> [Group] -> UserDB -> LoggedApplication env m
serveFlakePath route ref _allowedGroups _authDB _req respond = do
  logInfo $ "Serving route (authorized): " <> route

  let dataDir = "data"
      routeDir = dataDir </> T.unpack route
      htmlFile = routeDir </> T.unpack "index.html"
      pdfFile = routeDir </> T.unpack "main.pdf"

  _ <- liftIO $ async $
    usingLoggerT coloredLogAction $
    buildFlakeWithLogging route ref

  pdfExists <- liftIO $ doesFileExist pdfFile
  htmlExists <- liftIO $ doesFileExist htmlFile

  if htmlExists || pdfExists
    then do
      let finalFile = if pdfExists then "pdf-viewer.html" else htmlFile
      content <- liftIO $ TIO.readFile finalFile
      let script = websocketScriptFor route
      let finalOutput = content `T.append` script
      liftIO $ respond $ responseLBS status200 [("Content-Type", "text/html")] (BL.fromStrict $ encodeUtf8 finalOutput)
    else liftIO $ notFound respond

serve :: Text -> RouteMap -> UserDB -> LoggedApplication env m
serve normalizedPath routeMap authDB req respond = do
  logInfo $ "Serving " <> normalizedPath
  if Map.member normalizedPath routeMap
    then uncurry (serveFlakePath normalizedPath) (routeMap Map.! (normalizedPath <> "/")) authDB req respond
    else liftIO $ staticApp (defaultFileServerSettings "data") {ssAddTrailingSlash = True} req respond

serveWithCookie :: Text -> Header -> RouteMap -> UserDB -> LoggedApplication env m
serveWithCookie normalizedPath cookieHeader routeMap authDB req respond = do
  if Map.member (normalizedPath <> "/") routeMap
    then serveFlakeWithCookie normalizedPath cookieHeader routeMap authDB req respond
    else serveStaticWithCookie cookieHeader req respond

serveFlakeWithCookie :: Text -> Header -> RouteMap -> UserDB -> LoggedApplication env m
serveFlakeWithCookie normalizedPath cookieHeader routeMap authDB req respond = do
  let (ref, allowedGroups) = routeMap Map.! (normalizedPath <> "/")
  serveFlakePath normalizedPath ref allowedGroups authDB req $ \resp ->
    respond $ mapResponseHeaders ((cookieHeader :) . filter (\(h, _) -> h /= "Set-Cookie")) resp

serveStaticWithCookie :: Header -> LoggedApplication env m
serveStaticWithCookie cookieHeader req respond = do
  liftIO $ staticApp (defaultFileServerSettings "data") {ssAddTrailingSlash = True} req $ \r ->
    respond $ mapResponseHeaders ((cookieHeader :) . filter (\(h, _) -> h /= "Set-Cookie")) r

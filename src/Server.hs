{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Server
  ( serve,
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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.FileEmbed (embedFile)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Logger
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Application.Static (defaultFileServerSettings, ssAddTrailingSlash, staticApp)
import System.Directory
import System.FilePath ((</>))
import UnliftIO (async, MonadUnliftIO)

type LoggedApplication env m =
  (WithLog env Message m, MonadIO m, MonadUnliftIO m) =>
  Network.Wai.Request ->
  (Network.Wai.Response -> IO ResponseReceived) ->
  m ResponseReceived

pdfViewerHtml :: BS.ByteString
pdfViewerHtml = $(embedFile "pdf-viewer.html")

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

  _ <- async $ buildFlakeWithLogging route ref

  pdfExists <- liftIO $ doesFileExist pdfFile
  htmlExists <- liftIO $ doesFileExist htmlFile

  if htmlExists || pdfExists
    then do
      finalOutput <-
        if pdfExists
          then pure $ TE.decodeUtf8 pdfViewerHtml `T.append` websocketScriptFor route
          else do
            content <- liftIO $ TIO.readFile htmlFile
            pure $ content `T.append` websocketScriptFor route

      liftIO $ respond $ responseLBS status200 [("Content-Type", "text/html")] (BL.fromStrict $ encodeUtf8 finalOutput)
    else liftIO $ notFound respond

serve :: Text -> RouteMap -> UserDB -> LoggedApplication env m
serve normalizedPath routeMap authDB req respond = do
  logInfo $ "Serving " <> normalizedPath
  if Map.member normalizedPath routeMap
    then uncurry (serveFlakePath normalizedPath) (routeMap Map.! (normalizedPath <> "/")) authDB req respond
    else liftIO $ staticApp (defaultFileServerSettings "data") {ssAddTrailingSlash = True} req respond

serveFlakeWithCookie :: Text -> Header -> RouteMap -> UserDB -> LoggedApplication env m
serveFlakeWithCookie normalizedPath cookieHeader routeMap authDB req respond = do
  let (ref, allowedGroups) = routeMap Map.! (normalizedPath <> "/")
  serveFlakePath normalizedPath ref allowedGroups authDB req $ \resp ->
    respond $ mapResponseHeaders ((cookieHeader :) . filter (\(h, _) -> h /= "Set-Cookie")) resp

serveStaticWithCookie :: Header -> LoggedApplication env m
serveStaticWithCookie cookieHeader req respond = do
  liftIO $ staticApp (defaultFileServerSettings "data") {ssAddTrailingSlash = True} req $ \r ->
    respond $ mapResponseHeaders ((cookieHeader :) . filter (\(h, _) -> h /= "Set-Cookie")) r

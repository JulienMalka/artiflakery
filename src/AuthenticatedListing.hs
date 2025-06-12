{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module AuthenticatedListing (authenticatedListing) where

import Auth
import Config
import Control.Monad (filterM)
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.FileEmbed (embedFile)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types
import Network.Wai
import Server

nextSubDir :: T.Text -> T.Text -> Maybe T.Text
nextSubDir prefix full =
  case T.stripPrefix prefix full of
    Just rest ->
      case T.splitOn "/" rest of
        (next : _) | not (T.null next) -> Just (next <> "/")
        _ -> Nothing
    Nothing -> Nothing

directoryViewerHtml :: BS.ByteString
directoryViewerHtml = $(embedFile "directory-viewer.html")

makeFolderItem :: Text -> Text
makeFolderItem folder =
  T.unlines
    [ "                <li class=\"folder-item\">",
      "                    <a href=\"" <> cleanFolder <> "/\" class=\"folder-link\">",
      "                        <svg class=\"folder-icon\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\">",
      "                            <path d=\"M22 19a2 2 0 0 1-2 2H4a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h5l2 3h9a2 2 0 0 1 2 2z\"/>",
      "                        </svg>",
      "                        <span class=\"folder-name\">" <> cleanFolder <> "</span>",
      "                        <svg class=\"folder-arrow\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\">",
      "                            <path d=\"M9 18l6-6-6-6\"/>",
      "                        </svg>",
      "                    </a>",
      "                </li>"
    ]
  where
    cleanFolder = T.dropWhileEnd (== '/') folder

normalizePath :: T.Text -> T.Text
normalizePath path = ensureTrailingSlash (T.dropWhile (== '/') path)
  where
    ensureTrailingSlash p = if T.null p || T.isSuffixOf "/" p then p else p <> "/"

authenticatedListing :: RouteMap -> UserDB -> T.Text -> LoggedApplication env m
authenticatedListing routeMap _authDB requestedPath req respond = do
  let normPath = normalizePath requestedPath
      childRoutes =
        Map.toList $
          Map.filterWithKey
            (\route _ -> normPath `T.isPrefixOf` route && route /= normPath)
            routeMap

  authorized <- filterM (\(_, (_, groups)) -> fst <$> isAuthorized groups req) childRoutes

  let nextDirs = Set.fromList $ mapMaybe (nextSubDir normPath . fst) authorized
      links = Set.toList nextDirs
      template = TE.decodeUtf8 directoryViewerHtml
      folderItems = T.unlines $ map makeFolderItem links
      normPath' = (T.dropWhileEnd (== '/') normPath)
      path = if normPath' == "" then "" else "&mdash; <span class=\"path\">" <> normPath' <> "</span>"
      title = if normPath' == "" then "" else "&mdash; " <> normPath'
      html =
        T.replace "{{PATH}}" path $
          T.replace "{{TITLE}}" title $
            T.replace "{{FOLDERS}}" folderItems $
              template

  liftIO $ respond $ responseLBS status200 [("Content-Type", "text/html")] $ LBS.pack (T.unpack html)

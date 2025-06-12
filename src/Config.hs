module Config
  ( parseConfigFile,
    Route,
    FlakeRef,
    RouteMap,
    Group,
    UserId,
    PasswordHash,
    UserDB,
    parseAuthFile,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Route = Text

type FlakeRef = Text

type Group = Text

type UserId = Text

type PasswordHash = Text

type UserDB = Map Group [(UserId, PasswordHash)]

type RouteMap = Map Route (FlakeRef, [Group])

parseConfigFile :: FilePath -> IO RouteMap
parseConfigFile path = do
  content <- TIO.readFile path
  let parsed = mapMaybe parseLine (T.lines content)
      routeMap = Map.fromList [(route, (ref, groups)) | (route, ref, groups) <- parsed]
  return routeMap
  where
    parseLine :: Text -> Maybe (Route, FlakeRef, [Group])
    parseLine line =
      case T.words line of
        (route : ref : groups) -> Just (route, ref, groups)
        _ -> Nothing

parseAuthFile :: FilePath -> IO UserDB
parseAuthFile path = do
  content <- readFile path
  let entries = map (parseLine . T.strip . T.pack) (lines content)
      insertEntry m (g, u, h) = Map.insertWith (++) g [(u, h)] m
  return $ foldl insertEntry Map.empty (mapMaybe id entries)
  where
    parseLine :: Text -> Maybe (Group, UserId, PasswordHash)
    parseLine line =
      case T.splitOn "," line of
        [group, userHash] ->
          case T.splitOn ":" userHash of
            [user, hash] -> Just (group, user, hash)
            _ -> Nothing
        _ -> Nothing

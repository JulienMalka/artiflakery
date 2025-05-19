{-# LANGUAGE FlexibleContexts #-}

module Auth
  ( Group,
    UserId,
    UserDB,
    parseAuthFile,
    checkBasicAuth,
    isAuthorized,
    signCookieValue,
    verifyCookieValue,
    extractGroupsFromCookie,
    makeSecureCookieHeader,
    getSecretKey
  )
where

import Colog.Message
import Colog.Monad
import Config (Group, PasswordHash, UserDB, UserId, parseAuthFile)
import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import qualified Crypto.BCrypt as BCrypt
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock
import Network.HTTP.Types (Header, hAuthorization)
import Network.Wai (Request, requestHeaders)
import Web.Cookie
import System.Directory (doesFileExist)
import Crypto.Random (getRandomBytes)
import Control.Monad.IO.Class

-- | Load or generate a secret key for signing cookies
getSecretKey :: IO BS.ByteString
getSecretKey = do
  let keyFile = "secret-key"
  exists <- doesFileExist keyFile
  if exists
    then BS.readFile keyFile
    else do
      key <- getRandomBytes 32  -- Secure 256-bit key
      BS.writeFile keyFile key
      return key

-- | Check if a user is authorized based on group membership
isAuthorized :: (WithLog env Message m, MonadIO m) => [Group] -> Request -> m (Bool, [Group])
isAuthorized allowed req = do
  userGroups <- extractGroupsFromCookie req
  let userGroups' = userGroups ++ [ "public" ]
  let status = any (`elem` userGroups') allowed
  return (status, userGroups')

-- | Extract user groups from signed cookie
extractGroupsFromCookie :: (WithLog env Message m, MonadIO m) => Request -> m [Group]
extractGroupsFromCookie req = do
  secret <- liftIO getSecretKey
  let mCookieHeader = lookup "Cookie" (requestHeaders req)
  case mCookieHeader of
    Just raw ->
      let cookies = parseCookies raw
          mVal = lookup "authGroups" cookies >>= verifyCookieValue secret
       in case mVal of
            Just val -> do
              logDebug $ "COOKIE:" <> TE.decodeUtf8 val
              return $ T.splitOn "," (TE.decodeUtf8 val)
            Nothing -> return []
    Nothing -> return []

-- | Attempt to authenticate a user with Basic Auth and return their group if authorized
checkBasicAuth :: UserDB -> [Group] -> Request -> IO (Maybe Group)
checkBasicAuth db allowed req =
  case lookup hAuthorization (requestHeaders req) of
    Just authHeader ->
      case decodeBasicAuth authHeader of
        Just (uid, pw) -> findValidGroupIO db allowed uid pw
        Nothing -> return Nothing
    Nothing -> return Nothing

-- | Bcrypt password verification
verifyPassword :: PasswordHash -> Text -> Bool
verifyPassword hash pw =
  BCrypt.validatePassword (TE.encodeUtf8 hash) (TE.encodeUtf8 pw)

-- | Monadic version of findValidGroup using bcrypt password checking
findValidGroupIO :: UserDB -> [Group] -> UserId -> Text -> IO (Maybe Group)
findValidGroupIO db allowed uid pw = go (filter (`elem` allowed) (Map.keys db))
  where
    go [] = return Nothing
    go (g:gs) =
      case lookup uid (Map.findWithDefault [] g db) of
        Just hashedPw ->
          if verifyPassword hashedPw pw
            then return (Just g)
            else go gs
        Nothing -> go gs

-- | Decode HTTP Basic Auth credentials
decodeBasicAuth :: BS.ByteString -> Maybe (UserId, Text)
decodeBasicAuth bs =
  let prefix = "Basic "
   in case BS8.stripPrefix prefix bs of
        Just b64 ->
          case B64.decode b64 of
            Right decoded ->
              let (user, rest) = BS8.break (== ':') decoded
               in if BS8.null rest
                    then Nothing
                    else Just (TE.decodeUtf8 user, TE.decodeUtf8 $ BS8.tail rest)
            Left _ -> Nothing
        Nothing -> Nothing

-- | Sign a cookie value with HMAC
signCookieValue :: BS.ByteString -> BS.ByteString -> BS.ByteString
signCookieValue secret val =
  let sig = hmacGetDigest (hmac secret val :: HMAC SHA256)
   in BS8.concat [val, "|", B64.encode (BS8.pack $ show sig)]

-- | Verify a signed cookie value
verifyCookieValue :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
verifyCookieValue secret signed =
  let (val, sigPart) = BS8.breakSubstring "|" signed
   in case BS8.stripPrefix "|" sigPart of
        Just b64sig ->
          let expected = B64.encode . BS8.pack . show $ hmacGetDigest (hmac secret val :: HMAC SHA256)
           in if b64sig == expected
                then Just val
                else Nothing
        Nothing -> Nothing

-- | One week cookie expiry
weekInSeconds :: NominalDiffTime
weekInSeconds = 7 * 24 * 60 * 60

-- | Create a secure Set-Cookie header
makeSecureCookieHeader :: Text -> IO Header
makeSecureCookieHeader group = do
  now <- getCurrentTime
  let weekFromNow = Just $ addUTCTime weekInSeconds now
      cookie =
        defaultSetCookie
          { setCookieName = BS8.pack "authGroups",
            setCookieValue = TE.encodeUtf8 group,
            setCookiePath = Just "/",
            setCookieExpires = weekFromNow,
            setCookieHttpOnly = True,
            setCookieSecure = True,
            setCookieSameSite = Just sameSiteStrict
          }
      renderedCookie :: BS.ByteString
      renderedCookie = BL.toStrict $ toLazyByteString $ renderSetCookie cookie
  return ("Set-Cookie", renderedCookie)

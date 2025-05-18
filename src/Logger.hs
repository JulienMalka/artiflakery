module Logger
  (coloredLogAction)
  where

import Colog
import Colog.Actions (logTextStdout)
import Data.Time.Clock (UTCTime)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.Console.ANSI (Color(..), ColorIntensity(..), ConsoleLayer(..), SGR(..), setSGRCode)
import Data.Text (Text)
import qualified Data.Text as T


padRight :: Int -> Char -> T.Text -> T.Text
padRight width char text = 
    let textLen = T.length text
    in if textLen >= width
       then text
       else text <> T.replicate (width - textLen) (T.singleton char)


colorBySeverity :: Severity -> T.Text -> T.Text
colorBySeverity sev txt = case sev of
    Debug   -> colorize Green txt
    Info    -> colorize Blue txt
    Warning -> colorize Yellow txt
    Error   -> colorize Red txt
  where
    colorize :: Color -> T.Text -> T.Text
    colorize c txt =
        T.pack (setSGRCode [SetColor Foreground Vivid c])
        <> txt
        <> T.pack (setSGRCode [Reset])


customMessageFormat :: Severity -> UTCTime -> T.Text -> T.Text
customMessageFormat sev time msg = 
    let formattedText = T.intercalate " | " 
            [ padRight 8 ' ' (T.pack $ show sev)
            , T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" time
            , msg
            ]
    in colorBySeverity sev formattedText


coloredLogAction :: LogAction IO Message
coloredLogAction = 
    filterBySeverity Debug msgSeverity $
    cmapM (\msg -> do
        time <- getCurrentTime
        pure $ customMessageFormat (msgSeverity msg) time (msgText msg)
    ) logTextStdout

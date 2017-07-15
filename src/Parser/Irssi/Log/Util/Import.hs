module Parser.Irssi.Log.Util.Import (
  importIrssiData,
  importIrssiDataPure
) where

import           Control.Monad.Trans.State

--import           Data.Aeson                 (eitherDecode)
import qualified Data.ByteString.Lazy       as BSL
--import qualified Data.ByteString.Lazy.Char8 as BCL
import           Data.Maybe                 (catMaybes)
--import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Time
import           System.IO

import           Parser.Irssi.Log.Regex
import           Parser.Irssi.Log.Types
--import Data.Time.LocalTime


-- | Import an irssi log file into a list of LogTypes
--
importIrssiData :: FilePath -> IO [(LocalTime, LogType)]
importIrssiData path = do
  h <- openFile path ReadMode
  hSetEncoding h latin1
  contents <- T.hGetContents h
  let log_data = T.lines contents
  let log_types = catMaybes $ map parseIrssiLineText log_data
  evalStateT (mapM importIrssiData' log_types)
    (read "Fri Mar 04 09:10:30 2011" :: LocalTime)

-- | clean this up. just getting it to work
importIrssiData' :: Monad m => LogType -> StateT LocalTime m (LocalTime, LogType)
importIrssiData' log_type = do
  case log_type of
    e@(LogOpen t) -> go t e
    e@(LogClose t) -> go t e
    e@(DayChange t) -> go t e
    e@(Join o _ _ _) -> go2 o e
    e@(Part o _ _ _) -> go2 o e
    e@(Quit o _ _ _) -> go2 o e
    e@(Kick o _ _ _) -> go2 o e
    e@(Nick o _ _) -> go2 o e
    e@(OwnNick o _) -> go2 o e
    e@(Nicks o _ _ _ _ _) -> go2 o e
    e@(Mode o _ _) -> go2 o e
    e@(Message o _ _ _) -> go2 o e
    e@(Action o _ _) -> go2 o e
    e@(Invalid) -> go2 (fromIntegral (0 :: Int)) e
    where
    go t e = do
      put t
      return (t, e)
    go2 :: Monad m => Offset -> t -> StateT LocalTime m (LocalTime, t)
    go2 o e = do
      v <- get
      return (replaceTime v o, e)

utcTimeZone :: TimeZone
utcTimeZone = (hoursToTimeZone 0)

--replaces the time of day for a particular date using the given nominal diff time
--CAVEAT: may not be entirely correct.. didn't research this much
replaceTime :: LocalTime -> NominalDiffTime -> LocalTime
replaceTime lt t =
  let fakeUTCTime = addUTCTime t (localTimeToUTC utcTimeZone lt)
  in utcToLocalTime utcTimeZone fakeUTCTime



-- | Import an irssi data into a list of LogTypes
--
importIrssiDataPure :: BSL.ByteString -> [LogType]
importIrssiDataPure _ = undefined

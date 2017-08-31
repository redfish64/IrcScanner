{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Parser.Irssi.Log.Regex (
  parseIrssiLineBS,
  parseIrssiLineText,
  logOpen,
  logClose,
  dayChange,
  join,
  part,
  quit,
  kick,
  nick,
  ownNick,
  nicks,
  mode,
  message,
  action,
  irssiTimestampToLocalTime,
  irssiDayChangeTimestampToTZLessTime
) where

import           Data.Maybe                 (listToMaybe, mapMaybe)

import qualified Data.ByteString.Lazy       as BSL
--import qualified Data.ByteString.Lazy.Char8 as BCL
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Data.Time
--import           Data.Time.Format

import           Text.Regex.PCRE

import           Parser.Irssi.Log.Types



-- | Parse irssi line
--
parseIrssiLineBS :: BSL.ByteString -> Either String LogType
parseIrssiLineBS _ = undefined



-- | Parse irssi line

parseIrssiLineText :: Text -> Maybe LogType
parseIrssiLineText text = listToMaybe $ mapMaybe (\fn -> fn text) [message, logOpen, logClose, dayChange, join, part, quit, kick, nick, ownNick, nicks, mode, action]



logOpen :: Text -> Maybe LogType
logOpen text = do
  (_,time:[]) <- matcher text "^(---) Log opened (.*)$"
  return $ LogOpen (irssiTimestampToLocalTime time)



-- | --- Log closed Fri Mar 04 09:10:30 2011
--
logClose :: Text -> Maybe LogType
logClose text = do
  (_,time:[]) <- matcher text "^(---) Log closed (.*)$"
  return $ LogClose (irssiTimestampToLocalTime time)



-- | --- Day changed Fri Mar 04 09:10:30 2011
--
dayChange :: Text -> Maybe LogType
dayChange text = do
  (_,time:[]) <- matcher text "^(---) Day changed (.*)$"
  return $ DayChange (irssiDayChangeTimestampToTZLessTime time)



join :: Text -> Maybe LogType
join text = do
  (off,nick':mask':message':[]) <- matcher text "^(\\d\\d:\\d\\d(?::\\d\\d)?)\\s?-!- (\\S+) \\[([^\\]]+)\\] has joined"
  return $ Join off nick' mask' message'



part :: Text -> Maybe LogType
part text = do
  (off,nick':mask':message':[]) <- matcher text "^(\\d\\d:\\d\\d(?::\\d\\d)?)\\s?-!- (\\S+) \\[([^\\]]+)\\] has left(?:[^\\[]*\\[([^\\]]*))?"
  return $ Part off nick' mask' message'



quit :: Text -> Maybe LogType
quit text = do
  (off,nick':mask':message':[]) <- matcher text "^(\\d\\d:\\d\\d(?::\\d\\d)?)\\s?-!- (\\S+) \\[([^\\]]+)\\] has quit(?:[^\\[]*\\[([^\\]]*))?"
  return $ Quit off nick' mask' message'



kick :: Text -> Maybe LogType
kick text = do
  (off,nick':kicker:message':[]) <- matcher text "^(\\d\\d:\\d\\d(?::\\d\\d)?)\\s?-!- (\\S+) was kicked from \\S by (\\S) \\[([^\\]]+)\\]$"
  return $ Kick off nick' kicker message'



nick :: Text -> Maybe LogType
nick text = do
  (off,old_nick:new_nick:[]) <- matcher text "^(\\d\\d:\\d\\d(?::\\d\\d)?)\\s?-!- (\\S+) is now known as (\\S+)$"
  return $ Nick off old_nick new_nick



ownNick :: Text -> Maybe LogType
ownNick text = do
  (off,nick':[]) <- matcher text "^(\\d\\d:\\d\\d(?::\\d\\d)?)\\W+You're now known as (\\S+)$"
  return $ OwnNick off nick'



nicks :: Text -> Maybe LogType
nicks text = do
  (off,total:ops:half_ops:voices:normal:[]) <- matcher text "^(\\d\\d:\\d\\d(?::\\d\\d)?)\\W+Irssi: \\S+ Total of (\\d+) nicks \\[(\\d+) ops, (\\d+) halfops, (\\d+) voices, (\\d+) normal\\]$"
  return $ Nicks off (r total) (r ops) (r half_ops) (r voices) (r normal)
  where
  r = read . T.unpack



mode :: Text -> Maybe LogType
mode text = do
  (off,modes:moder:[]) <- matcher text "^(\\d\\d:\\d\\d(?::\\d\\d)?)\\s?-!- (?:ServerM|m)ode\\/\\S+ \\[([^\\]]+)\\] by (\\S*)$"
  return $ Mode off [modes] moder



message :: Text -> Maybe LogType
message text = do
  (off,mode':nick':message':[]) <- matcher text "^(\\d\\d:\\d\\d(?::\\d\\d)?)\\s?<(.)([^>]+)> (.*)$"
  return $ Message off mode' nick' message'



action :: Text -> Maybe LogType
action text = do
  (off,nick':message':[]) <- matcher text "^(\\d\\d:\\d\\d(?::\\d\\d)?)\\s+\\* (\\S+) (.*)$"
  return $ Action off nick' message'



matcher :: Text -> String -> Maybe (Offset, [Text])
matcher text regex =
  case matches of
    (_:off:rest) -> Just (calcOffset off, rest)
    _            -> Nothing
  where
  matches' = getAllTextSubmatches (T.unpack text =~ regex :: AllTextSubmatches [] String)
  matches = map T.pack matches'



-- |
calcOffset :: Text -> Offset
calcOffset text =
  let hh = (read $ T.unpack (T.take 2 text) :: Int)
      mm = (read $ T.unpack (T.drop 3 text) :: Int)
      in fromIntegral (60 * (mm + hh * 60))



-- |
-- Fri Mar 04 09:10:30 2011
--
irssiTimestampToLocalTime :: Text -> LocalTime
irssiTimestampToLocalTime =
  (parseTimeOrError True defaultTimeLocale "%a %b %d %T %Y" . T.unpack)



-- | DayChange has a different format, lacks 00:00 offset
-- Day changed Tue Feb 16 2016
--
irssiDayChangeTimestampToTZLessTime :: Text -> LocalTime
irssiDayChangeTimestampToTZLessTime = (parseTimeOrError True defaultTimeLocale "%a %b %d %Y" . T.unpack)



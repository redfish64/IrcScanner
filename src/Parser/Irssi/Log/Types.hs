{-# LANGUAGE DeriveGeneric #-}

module Parser.Irssi.Log.Types (
  LogType (..),
  LogEntry,
  Offset,
  Nickname,
  Mask,
  Kicker,
  Total,
  Ops,
  HalfOps,
  Voices,
  Normal,
  Mode,
  Moder,
  MessageContent
) where

import           Data.Text     (Text)
import           Data.Time     (UTCTime, LocalTime)
import           Data.Typeable
import           GHC.Generics
--import   Data.Time.Calendar(Day)
import Data.Time.Clock (NominalDiffTime)

  

data LogType =
    LogOpen LocalTime
  | LogClose LocalTime
  | DayChange LocalTime
  | Join Offset Nickname Mask MessageContent
  | Part Offset Nickname Mask MessageContent
  | Quit Offset Nickname Mask MessageContent
  | Kick Offset Nickname Kicker MessageContent
  | Nick Offset Nickname Nickname
  | OwnNick Offset Nickname
  | Nicks Offset Total Ops HalfOps Voices Normal
  | Mode Offset [Mode] Moder
  | Message Offset Mode Nickname MessageContent
  | Action Offset Nickname MessageContent
  | Invalid
  deriving (Eq, Show, Ord, Generic, Typeable)

type LogEntry = (UTCTime, LogType)

type Offset = NominalDiffTime
type Nickname = Text
type Mask = Text
type Kicker = Text
type Total = Int
type Ops = Int
type HalfOps = Int
type Voices = Int
type Normal = Int
type Mode = Text
type Moder = Text
type MessageContent = Text


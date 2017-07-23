{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module IrcScanner.Types(Matcher,Index(..),Pos(..),Range(..),CachedIndexResult(..),emptyCacheIndexResult, MatcherType(..),mkMatcher,IState(..),IConfig(..),IST,EIST,emptyIState,ILine(..),
            idisplayName,
            imatcher,
            prow,
            pcol,
            rstartPos,
            rendPos,
            cindex,
            cranges,
            cendLine,
            scirs,
            sfile,
            cstate,
            IrcSnaplet(..),
            iheist,
            iconfig,
            ltime,
            llogType,
            fcurrDay,
            flines,
            ctimeZone,
            hacktest
            ) where

import Data.Text.ICU as I
import Control.Monad.Trans.Reader

import Data.Sequence as S
import Data.Text
import Test.Hspec
import Data.Either(isLeft)
import IrcScanner.Util
import Data.IORef
import Control.Lens.TH
import Control.Monad.Trans.Either(EitherT)
import           Snap
import           Snap.Snaplet.Heist
import Data.Time.Clock
import Data.Time.Calendar(Day,fromGregorian)
import Data.Time.LocalTime
import Parser.Irssi.Log.Types(LogType)

data MatcherType = ExactMatcher | IgnoreCaseMatcher | RegexMatcher
   deriving (Show, Eq)

type Matcher = Regex 

mkMatcher :: MatcherType -> Text -> (Either Text Matcher)
mkMatcher _ "" = Left "Empty val not allowed."
mkMatcher IgnoreCaseMatcher val = mkRegexMatcher [CaseInsensitive, Literal] val
mkMatcher ExactMatcher val = mkRegexMatcher [Literal] val
mkMatcher RegexMatcher val =
    parseRegexWithFlags val
  
mkRegexMatcher :: [MatchOption] -> Text -> (Either Text Matcher)
mkRegexMatcher mo r = replaceLeft (pack . show) (regex' mo r)


_test :: IO ()
_test =
  hspec $ do
  describe "mkMatcher" $ do
    it "handles exact match" $ do
      pattern <$> (mkMatcher ExactMatcher "foo") 
        `shouldBe` Right "foo"
    it "handles Regex Match" $ do
      pattern <$>
        (mkMatcher RegexMatcher "/foo/") 
        `shouldBe` Right "foo"
    it "handles bad regex match" $ do
      pattern <$>
        (mkMatcher RegexMatcher "/foo(/") 
        `shouldSatisfy` isLeft
  
      


--this is used to form keywords agains the logger
data Index = Index {
  _idisplayName :: Text,
  _imatcher :: Matcher
  } deriving (Show)

makeLenses ''Index


data Pos = Pos {
  _prow :: Int,
  _pcol :: Int
  } deriving (Show, Eq)

makeLenses ''Pos

data Range = Range {
  _rstartPos :: Pos,
  _rendPos :: Pos
  } deriving (Show, Eq)

makeLenses ''Range


--This is a cache of results from running indexes against
--the log file
--we don't use straight memoization because we need to
--incremenetally update the results when new lines come in
data CachedIndexResult = CachedIndexResult {
  _cindex :: Index,
  _cranges :: Seq Range,
  _cendLine :: Int -- the line number up to which we have scanned
  } deriving (Show)

makeLenses ''CachedIndexResult


emptyCacheIndexResult :: Index -> CachedIndexResult
emptyCacheIndexResult i = CachedIndexResult i S.empty 0

data ILine = ILine {
  _ltime :: UTCTime,
  _llogType :: LogType
  } deriving (Show)

makeLenses ''ILine

data IFile = IFile {
  _flines :: Seq ILine,
  _fcurrDay :: Day   --latest day in the file
            -- (in irssi format, only the time of lines are displayed, and when the day changes, a line indicating the fact is printed)
  } deriving (Show)

makeLenses ''IFile

data IState = IState {
  _scirs :: [CachedIndexResult],
  _sfile :: IFile --eventually maybe we'll support multiple files
  } deriving (Show)

makeLenses ''IState

emptyIState :: IState
emptyIState = IState { _scirs = [], _sfile = IFile { _flines = S.empty, _fcurrDay = fromGregorian 1970 1 1 } }

data IConfig = IConfig
  {
    _cstate :: IORef IState,
    _ctimeZone :: TimeZone
  }

makeLenses ''IConfig  

type IST = ReaderT IConfig
type EIST a = EitherT Text (IST a)

data IrcSnaplet = IrcSnaplet {
  _iheist :: Snaplet (Heist IrcSnaplet),
  _iconfig :: IConfig,
  _hacktest :: Integer
  }

makeLenses ''IrcSnaplet

instance HasHeist IrcSnaplet where
  heistLens = subSnaplet iheist
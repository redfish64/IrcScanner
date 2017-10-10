{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module IrcScanner.Types(Matcher,Index(..),Pos(..),Range(..),CachedIndexResult(..),emptyCacheIndexResult, MatcherType(..),mkMatcher,IState(..),IConfig(..),IST,EIST,emptyIState,ILine(..),IFile(..),
            idisplayName,
            imatcher,
            prow,
            pcol,
            rstartPos,
            rendPos,
            rfnn,
            cindex,
            cranges,
            cfnnEndLine,
            scirs,
            emptyFile,
            sfiles,
            cstate,
            IrcSnaplet(..),
            iheist,
            iconfig,
            ltime,
            llogType,
            fcurrDay,
            flines,
            ctimeZone,
            crulesFile,
            skwFileContents,
            ckwFileLock
            ) where

import Data.Map as M
import Data.Text.ICU as I
import Control.Monad.Trans.Reader
import Control.Concurrent.MVar
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
   deriving (Show, Eq, Read)

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
  _rfnn :: Text,   -- ^ the file nick name the range appeared in
  _rstartPos :: Pos,
  _rendPos :: Pos
  } deriving (Show, Eq)

makeLenses ''Range


{- | This is a cache of results from running indexes against
     the log file
     we don't use straight memoization because we need to
     incremenetally update the results when new lines come in
 -}
data CachedIndexResult = CachedIndexResult {
  _cindex :: Index,
  _cranges :: Seq Range,
  _cfnnEndLine :: Map Text Int -- ^ the line number up to which we have scanned for each fnn
  } deriving (Show)

makeLenses ''CachedIndexResult


emptyCacheIndexResult :: Index -> CachedIndexResult
emptyCacheIndexResult i = CachedIndexResult i S.empty M.empty

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
  _scirs :: [CachedIndexResult], -- ^ this is the cached result of all keyword scans
  _sfiles :: Map Text IFile, -- ^ files we read from, as name to file
  _skwFileContents :: Text -- ^ this is the raw source of the file configuring which keywords to search for
  } deriving (Show)

makeLenses ''IState

emptyFile :: IFile
emptyFile = IFile { _flines = S.empty, _fcurrDay = fromGregorian 1970 1 1 }

emptyIState :: IState
emptyIState = IState { _scirs = [], _sfiles = M.fromList [], _skwFileContents = "" }

data IConfig = IConfig
  {
    _cstate :: IORef IState,
    _ctimeZone :: TimeZone,
    _crulesFile :: FilePath,
    _ckwFileLock :: MVar ()
  }

makeLenses ''IConfig  

type IST = ReaderT IConfig
type EIST a = EitherT Text (IST a)

data IrcSnaplet = IrcSnaplet {
  _iheist :: Snaplet (Heist IrcSnaplet),
  _iconfig :: IConfig
  }

makeLenses ''IrcSnaplet

instance HasHeist IrcSnaplet where
  heistLens = subSnaplet iheist

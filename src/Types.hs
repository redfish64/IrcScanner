module Types(Matcher,Index(..),Pos(..),Range(..),CachedIndexResult(..),emptyCacheIndexResult, MatcherType(..),mkMatcher,IState(..),IConfig(..),IST,emptyIState) where

import Data.Text.ICU as I
import Control.Monad.Trans.Reader

import Data.Sequence as S
import Data.Text
import Test.Hspec
import Data.Either(isLeft)
import Util
import Data.IORef

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
  idisplayName :: Text,
  imatcher :: Matcher
  } deriving (Show)


data Pos = Pos {
  prow :: Int,
  pcol :: Int
  } deriving (Show, Eq)

data Range = Range {
  rstartPos :: Pos,
  rendPos :: Pos
  } deriving (Show, Eq)



--This is a cache of results from running indexes against
--the log file
--we don't use straight memoization because we need to
--incremenetally update the results when new lines come in
data CachedIndexResult = CachedIndexResult {
  cindex :: Index,
  cranges :: Seq Range,
  cendLine :: Int -- the line number up to which we have scanned
  } deriving (Show)


emptyCacheIndexResult :: Index -> CachedIndexResult
emptyCacheIndexResult i = CachedIndexResult i S.empty 0

data IState = IState {
  rcirs :: [CachedIndexResult],
  rfile :: Seq Text
  } deriving (Show)

emptyIState :: IState
emptyIState = IState { rcirs = [], rfile = S.empty }

data IConfig = IConfig
  {
    cstate :: IORef IState
  } 

type IST = ReaderT IConfig


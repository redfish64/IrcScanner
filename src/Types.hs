module Types(Matcher,Index(..),Pos(..),Range(..),CachedIndexResult(..),IndexResult, MatcherType(..),mkMatcher,IState(..),IConfig(..),ILog,IST) where

import Data.Text.ICU 
--import qualified Data.Text.ICU.Regex as R
import Control.Monad.Trans.RWS

import Data.DList 
import Data.Sequence
import Data.Text
import Text.Parsec.Text

data MatcherType = ExactMatcher | IgnoreCaseMatcher | RegexMatcher
   deriving (Show, Eq)

type Matcher = Regex 

mkMatcher :: MatcherType -> Text -> (Either Text Matcher)
mkMatcher _ "" = Left "Empty val not allowed."
mkMatcher IgnoreCaseMatcher val = mkRegexMatcher [CaseInsensitive, Literal] val
mkMatcher ExactMatcher val = mkRegexMatcher [Literal] val
mkMatcher RegexMatcher val =
  do
    (opts,r) <- parseRegexWithFlags val
    mkRegexMatcher opts r

    
--parses a regex with basic match options, etc.
parseRegexWithFlags :: Text -> Either Text ([MatchOption],Text)
parseRegexWithFlags text =
  runParser myParser
  where
    myParser :: Parser ([MatchOption], Text)
    myParser =
      do
        s <- anyChar
        r <- pack $ many anyChar
        char s
        flags <- choice (Prelude.map char "i") --right now there is only one option, ignore case
        return (parseFlag flags, s)
    parseFlags :: Char -> MatchOption
    parseFlags 'i' = Right CaseInsensitive
      

mkRegexMatcher :: [MatchOption] -> Text -> (Either Text Matcher)
mkRegexMatcher mo r = replaceLeft (pack . show) (regex' mo r)
  
      


replaceLeft :: (a -> b) -> Either a c -> Either b c
replaceLeft f e =
    case e of
      Left x -> Left $ f x
      Right y -> Right y

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


--if Left, means there was a 
type IndexResult = Either Text [Range]


--This is a cache of results from running indexes against
--the log file
--we don't use straight memoization because we need to
--incremenetally update the results when new lines come in
data CachedIndexResult = CachedIndexResult {
  cindex :: Index,
  cranges :: [Range],
  cendLine :: Int -- the line number we have checked through 
  } deriving (Show)



data IState = IState {
  rcirs :: [CachedIndexResult],
  rfile :: Seq Text
  }

type ILog = DList Text

data IConfig = IConfig { } 

type IST = RWST IConfig ILog IState 


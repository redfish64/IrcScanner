module Types(Matcher(mtype,mval),Index(..),Pos(..),Range(..),CachedIndexResult(..),IndexResult, MatcherType(..),mkMatcher) where


import Text.Regex.PCRE

data MatcherType = ExactMatcher | IgnoreCaseMatcher | RegexMatcher
   deriving (Show, Eq)

data Matcher = Matcher { mtype :: MatcherType, mval :: String } deriving (Show, Eq)

mkMatcher :: MatcherType -> String -> Either String Matcher
mkMatcher _ "" = Left "Empty val not allowed."
mkMatcher RegexMatcher val =
  case (testRegex val) of
    Nothing -> Left "Can't parse regex"
    Just () -> Right $ Matcher RegexMatcher val
  where
    --make sure the regex is useable
    testRegex :: String -> Maybe ()
    testRegex r =
      do 
        (("foo" :: String) =~~ r :: Maybe Bool)
        return ()
mkMatcher t v = Right (Matcher t v)
        

--this is used to form keywords agains the logger
data Index = Index {
  idisplayName :: String,
  imatcher :: Matcher
  } deriving (Show, Eq)


data Pos = Pos {
  prow :: Int,
  pcol :: Int
  } deriving (Show, Eq)

data Range = Range {
  rstartPos :: Pos,
  rendPos :: Pos
  } deriving (Show, Eq)


--if Left, means there was a 
type IndexResult = Either String [Range]


--This is a cache of results from running indexes against
--the log file
--we don't use straight memoization because we need to
--incremenetally update the results when new lines come in
data CachedIndexResult =
  CachedIndexResult {
    cranges :: [Range],
    clastLine :: Int -- the line number we have checked through 
    } deriving (Show)


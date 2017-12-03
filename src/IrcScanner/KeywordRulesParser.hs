{-# LANGUAGE OverloadedStrings #-}
module IrcScanner.KeywordRulesParser(parseKwFile,saveKwFile) where


import Text.ParserCombinators.ReadP
import IrcScanner.Types
import Text.Read(readMaybe)
import qualified Data.Text as T
import Test.Hspec
import Control.Monad.Trans.Either
import Control.Monad.State as S
import Control.Concurrent.MVar
import System.Directory(renameFile)
import Data.Text.IO as T(writeFile)
--import Control.Monad(sequence)
-- ex file
-- Nomiccoin:RegexMatcher:/\bnomiccoin\b|\ba\.?n\.?o\.?n\b/
-- Cool:RegexMatcher:/\bcool\b/i
-- Open/Closed Phase:RegexMatcher:/\b(open|closed?)\s+phase\b/i


--accepts any character following a backslash
backSlashChar :: ReadP String
backSlashChar =
  do
    _ <- char '\\'
    c <- satisfy $ const True --read any character
    return $ "\\"++[c]

--allows for the backslashing of a char:
--  if the match is "\<char>", then "<char>" is returned
--  if "\<other char>", then "\<other char>" is returned
--  if <char>, fails
--  if <other char>, "<other char>" is returned
backSlashableSep :: Char -> ReadP String
backSlashableSep m = (choice [string ("\\"++ [m]) >> return [m],
                              backSlashChar,
                              (satisfy (\c-> c /= m))>>= return . (: [])
                             ])

field :: ReadP T.Text
field =
  do
    f <- many1 $ backSlashableSep ':'
    choice [char ':' >> return (), eof]
    return $ T.pack (foldr (++) [] f)

matchType :: ReadP MatcherType
matchType =
  do
    s <- field
    
    case (readMaybe $ T.unpack s) of
      Nothing -> pfail
      Just x -> return x

matcher :: MatcherType -> ReadP Matcher
matcher mt =
  do
    pat <- field
    case mkMatcher mt pat of
      Left _ -> pfail
      Right x -> return x
    
      
--either reads its value or returns an error message specified by errorFunc
maybeRead :: ReadP x -> (T.Text -> T.Text) -> EitherT T.Text (State T.Text) x
maybeRead readp errorFunc =
  do
    s <- S.get
    v <- return $ readP_to_S readp (T.unpack s)
    case v of
      (x,y) : _ -> put (T.pack y) >> return x
      _ -> left $ errorFunc s


kwLine :: EitherT T.Text (State T.Text) Index
kwLine =
  do
    dn <- maybeRead field $ const "Display name must have at least one char"
    mt <- maybeRead matchType (T.append "Can't interpret matchtype: ")
    m <- maybeRead (matcher mt) (T.append "Can't parse pattern: ")
    maybeRead eof $ (T.append "Extra characters at end of line: ")
    return $ Index dn m


-- kwFile :: [T.Text] -> [Either T.Text Index]
-- kwFile lns = fmap (evalState $ runEitherT kwLine) lns 

parseKwFile :: [T.Text] -> Either [T.Text] [Index]
parseKwFile lns = listEitherToEitherLists (fmap (evalState $ runEitherT kwLine) lns ) "" 


saveKwFile :: T.Text -> IConfig -> IO ()
saveKwFile contents ic =
  let
    fileName = (_crulesFile ic)
    bakFileName = fileName ++ ".bak"
    lock = (_ckwFileLock ic)
  in
    withMVar lock $ const $ do
      renameFile fileName bakFileName
      T.writeFile fileName contents




--if there are any lefts, returns a list of the lefts. For rows that are right in that
-- case, returns placeholder
--otherwise returns a list of the rights
listEitherToEitherLists :: [Either a b] -> a -> Either [a] [b]
listEitherToEitherLists lns placeholder = case doit lns (Right []) of
  Left x -> Left (reverse x)
  Right y -> Right (reverse y)
  where
    doit (Right x : xs) (Right ys) = doit xs (Right (x : ys))
    doit (Left y : ys) (Right xs) = doit ys (Left $ y : (fmap (const placeholder) xs))
    doit (Right _ : xs) (Left ys) = doit xs (Left $ placeholder : ys)
    doit (Left y : xs) (Left ys) = doit xs $ Left (y : ys)
    doit [] r = r
    
      
    
_test :: IO ()
_test =
  hspec $ do
  describe "kwLine" $ do
    it "the happy path" $ do
      runState (runEitherT kwLine ) "Nomiccoin:RegexMatcher:/\\bnomiccoin\\b|\\ba\\.?n\\.?o\\.?n\\b/"
        `shouldSatisfy`
        (\(v,_) -> 
           case v of
             (Right x) -> (show x) == "Index {_idisplayName = \"Nomiccoin\", _imatcher = Regex \"\\\\bnomiccoin\\\\b|\\\\ba\\\\.?n\\\\.?o\\\\.?n\\\\b\"}"
             _ -> False
        )
  describe "kwFile" $ do
    it "the happy path" $ do
      (show $ parseKwFile ["Nomiccoin:RegexMatcher:/\\bnomiccoin\\b|\\ba\\.?n\\.?o\\.?n\\b/","Cool:RegexMatcher:/\\bcool\\b/i"])
        `shouldBe`
        "Right [Index {_idisplayName = \"Cool\", _imatcher = Regex \"\\\\bcool\\\\b\"},Index {_idisplayName = \"Nomiccoin\", _imatcher = Regex \"\\\\bnomiccoin\\\\b|\\\\ba\\\\.?n\\\\.?o\\\\.?n\\\\b\"}]"
   

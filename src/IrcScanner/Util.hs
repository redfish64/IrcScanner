{-# LANGUAGE OverloadedStrings #-}
module Util(replaceLeft,parseRegexWithFlags,justOrError) where

import Data.Text
import Data.Text.ICU
import Text.Parsec
import Text.Parsec.Text
import Test.Hspec
import Data.Either(isLeft)



replaceLeft :: (a -> b) -> Either a c -> Either b c
replaceLeft f e =
    case e of
      Left x -> Left $ f x
      Right y -> Right y

replaceRight :: (a -> b) -> Either c a -> Either c b
replaceRight f e =
    case e of
      Left x -> Left x
      Right y -> Right $ f y

--parses a regex with basic match options, etc.
parseRegexWithFlags :: Text -> Either Text Regex
parseRegexWithFlags text =
  either (Left . pack . show)
  (\(flags,r) -> replaceLeft (pack . show) $ regex' flags r)  $ runParser myParser () "(unknown)" text 
  where
    myParser :: Parser ([MatchOption], Text)
    myParser =
      do
        s <- anyChar
        r <- many (try (char '\\' >> (char s)) <|> noneOf [s])
        char s
        flags <- many $ choice (Prelude.map char "i") --right now there is only one option, ignore case
        eof
        return (fmap parseFlag flags, pack r)
    parseFlag :: Char -> MatchOption
    parseFlag 'i' = CaseInsensitive
    parseFlag _ = undefined
      


_test :: IO ()
_test =
  hspec $ do
  describe "parseRegexWithFlags" $ do
    it "parses a normal regex" $ do
      (replaceRight show (parseRegexWithFlags "/foo/"))
        `shouldBe` Right "Regex \"foo\""
    it "handles escape of separator and 'i'" $ do
      (replaceRight show (parseRegexWithFlags "f\\foofi"))
        `shouldBe` Right "Regex \"foo\""
    it "handles arbitrary escapes" $ do
      (replaceRight show (parseRegexWithFlags "f\\foo\\gf"))
        `shouldBe` Right "Regex \"foo\\\\g\""
    it "fails properly" $ do
         (parseRegexWithFlags "f\\fooi")
        `shouldSatisfy` isLeft

justOrError :: Text -> Maybe x -> Either Text x
justOrError s Nothing = Left s
justOrError _ (Just x) = Right x

 
-- readFile :: Text -> EitherT IO Text
-- readFile = 


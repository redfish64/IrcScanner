{-# LANGUAGE OverloadedStrings #-}
module IrcScanner.Util(replaceLeft,parseRegexWithFlags,justOrError,sliceSeq,readOrLeft,mapInd) where

import Data.Text
import Data.Text.ICU
import Text.Parsec
import Text.Parsec.Text
import Test.Hspec
import Data.Either(isLeft)
import qualified Data.Sequence as S
import Control.Monad.Trans.Either(EitherT(..))
import Text.Read(readMaybe)

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

sliceSeq :: Int -> Int -> S.Seq x -> S.Seq x
sliceSeq from cnt xs = S.take cnt (S.drop from xs)

readOrLeft :: (Monad x, Read y) => Text -> Text -> EitherT Text x y
readOrLeft errMsg val = EitherT $ return (justOrError errMsg $ readMaybe (unpack val))

-- readFile :: Text -> EitherT IO Text
-- readFile = 

-- variant of map that passes each element's index as a second argument to f
mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = Prelude.zipWith f l [0..]

module Test where

import Test.Hspec

import Types

testTypes :: IO ()
testTypes =
  hspec $ do
  describe "Types" $ do
    it "mkMatcher" $ do
      mtype <$>
        (mkMatcher ExactMatcher "foo") 
        `shouldBe` Right ExactMatcher
      mtype <$>
        (mkMatcher RegexMatcher "foo") 
        `shouldBe` Right RegexMatcher
      mtype <$>
        (mkMatcher RegexMatcher "foo(") 
        `shouldSatisfy` (\x -> case x of
                                 Left _ -> True
                                 _ -> False)


  

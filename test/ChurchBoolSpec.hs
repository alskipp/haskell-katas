{-# Language RankNTypes #-}

module ChurchBoolSpec where

import Test.Hspec
import Prelude hiding (not,and,or,(&&),(||),(==),(/=))

type Boolean = forall a. a -> a -> a -- this requires RankNTypes

false,true :: Boolean
false = \ t f -> f
true  = \ t f -> t

not :: Boolean -> Boolean
and,or,xor :: Boolean -> Boolean -> Boolean

not = flip
and = \ a b -> a b false
or  = \ a b -> a true b
xor = \ a b -> a (not b) b

unchurch :: Boolean -> Bool
unchurch bool = bool True False


spec :: Spec
spec = do
  describe "example tests" $ do
    it "not" $ do
      unchurch (not false) `shouldBe` True
      unchurch (not true) `shouldBe` False
    it "and" $ do
      unchurch (false `and` false) `shouldBe` False
      unchurch (false `and` true) `shouldBe` False
      unchurch (true `and` false) `shouldBe` False
      unchurch (true `and` true) `shouldBe` True
    it "or" $ do
      unchurch (false `or` false) `shouldBe` False
      unchurch (false `or` true) `shouldBe` True
      unchurch (true `or` false) `shouldBe` True
      unchurch (true `or` true) `shouldBe` True
    it "xor" $ do
      unchurch (false `xor` false) `shouldBe` False
      unchurch (false `xor` true) `shouldBe` True
      unchurch (true `xor` false) `shouldBe` True
      unchurch (true `xor` true) `shouldBe` False

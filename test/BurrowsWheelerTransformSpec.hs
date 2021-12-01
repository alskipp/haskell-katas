{-# LANGUAGE TupleSections #-}

module BurrowsWheelerTransformSpec where

import Data.List (elemIndex, sort)
import Test.Hspec

-- | Encode an input sequence with the Burrows-Wheeler-Transformation.
encode :: Ord a => [a] -> ([a], Int)
encode xs = maybe ([], 0) (bwt,) . elemIndex xs $ matrix
  where
    matrix = sort . take len $ iterate rotateR xs
    bwt = fmap last matrix
    rotateR = take len . drop (len - 1) . cycle
    len = length xs

-- | Get back the input from a Burrows-Wheeler-Transformation.
decode :: Ord a => [a] -> Int -> [a]
decode xs i = fmap (xs !!) . take len $ iterate (indices !!) (indices !! i)
  where
    len = length xs
    indices = fmap snd . sort $ zip xs [0 ..]

spec :: Spec
spec = do
  describe "basic examples for encode" $ do
    it "bananabar" $ encode "bananabar" `shouldBe` ("nnbbraaaa", 4)
    it "Humble Bundle" $ encode "Humble Bundle" `shouldBe` ("e emnllbduuHB", 2)
    it "Mellow Yellow" $ encode "Mellow Yellow" `shouldBe` ("ww MYeelllloo", 1)
  describe "basic examples for decode" $ do
    it "bananabar" $ uncurry decode ("nnbbraaaa", 4) `shouldBe` "bananabar"
    it "Humble Bundle" $
      uncurry decode ("e emnllbduuHB", 2)
        `shouldBe` "Humble Bundle"
    it "Mellow Yellow" $
      uncurry decode ("ww MYeelllloo", 1)
        `shouldBe` "Mellow Yellow"

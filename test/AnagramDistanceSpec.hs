{-# LANGUAGE TupleSections #-}

module AnagramDistanceSpec where

import qualified Data.Map.Lazy as M
import Test.Hspec

-- number of additions or removals to make the 2 inputs anagrams
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++
anagramDistance :: Ord a => [a] -> [a] -> Int
anagramDistance s t = diff (charCounts s) (charCounts t)
  where
    charCounts = M.fromListWith (+) . fmap (,1)
    diff a b = sum . M.elems $ M.unionWith (\x y -> abs (x - y)) a b

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++
spec :: Spec
spec = describe "anagramDistance" $ do
  it "test strings" $ do
    anagramDistance "hello" "helo" `shouldBe` 1
    anagramDistance "hello" "hela" `shouldBe` 3
    anagramDistance "cde" "abc" `shouldBe` 4
    anagramDistance "" "aba" `shouldBe` 3
    anagramDistance "" "" `shouldBe` 0

module DescendingOrderSpec where

import Data.Foldable (toList)
import Data.List
  ( foldl',
    sortBy,
  )
import Data.Ord
  ( Down (..),
    comparing,
  )
import qualified Data.Sequence as Seq
import Test.Hspec

descendingOrder :: Integer -> Integer
descendingOrder = digitsToInt . sortBy (comparing Down) . toDigits

toDigits :: Integer -> [Integer]
toDigits = toList . Seq.unfoldl f
  where
    f x = if x > 0 then Just (quotRem x 10) else Nothing

digitsToInt :: [Integer] -> Integer
digitsToInt = foldl' ((+) . (* 10)) 0

spec :: Spec
spec = do
  describe "descendingOrder" $ do
    it "should work on several examples" $ do
      descendingOrder 0 `shouldBe` 0
      descendingOrder 1 `shouldBe` 1
      descendingOrder 15 `shouldBe` 51
      descendingOrder 1021 `shouldBe` 2110
      descendingOrder 123456789 `shouldBe` 987654321

module PascalsTriangleSpec where

import Test.Hspec

pascalsTriangle :: Int -> [Int]
pascalsTriangle n =
  let (xs, ys) = foldr f ([1], []) (replicate (n - 1) 1) in xs ++ ys
  where
    f a (h, t) = (h ++ t, a : zipWith (+) t (tail t) ++ [a])

pascalsTriangle' :: Int -> [Int]
pascalsTriangle' n = concat . take n . iterate row $ [1]
  where
    row xs = 1 : zipWith (+) xs (tail xs) ++ [1]

verify :: (Int -> [Int]) -> Spec
verify f = it "Verify first 5 results" $ do
  f 1 `shouldBe` [1]
  f 2 `shouldBe` [1, 1, 1]
  f 3 `shouldBe` [1, 1, 1, 1, 2, 1]
  f 4 `shouldBe` [1, 1, 1, 1, 2, 1, 1, 3, 3, 1]
  f 5 `shouldBe` [1, 1, 1, 1, 2, 1, 1, 3, 3, 1, 1, 4, 6, 4, 1]

spec :: Spec
spec = describe "Test" $ do
  describe "Pascal tests v1" $ verify pascalsTriangle
  describe "Pascal tests v2" $ verify pascalsTriangle'

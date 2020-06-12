module BinaryTreeSpec where

import Data.List (sort)
import Test.Hspec
import Test.QuickCheck

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++
data Tree a = L | N (Tree a) a (Tree a) deriving (Show, Eq)

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++

-- tuple values indicating number of left & right paths taken
infTree :: Tree (Integer, Integer)
infTree = t 0 0
  where
    t l r = N (t (succ l) r) (l, r) (t l (succ r))

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++
-- depth to cut tree
cut :: Integer -> Tree a -> Tree a
cut 0 _ = L
cut _ L = L
cut i (N l x r) = let n = pred i in N (cut n l) x (cut n r)

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++
insert :: (Ord a) => a -> Tree a -> Tree a
insert a L = N L a L
insert a (N l x r)
  | a <= x = N (insert a l) x r
  | otherwise = N l x (insert a r)

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++
inOrder :: (Ord a) => Tree a -> [a]
inOrder L = []
inOrder (N l x r) = inOrder l ++ x : inOrder r

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++
spec :: Spec
spec = describe "Binary Tree funcs" $ do
  it "cut inf tree" $ do
    cut 0 infTree `shouldBe` L
    cut 1 infTree `shouldBe` N L (0, 0) L
    cut 2 infTree `shouldBe` N (N L (1, 0) L) (0, 0) (N L (0, 1) L)
    cut 3 infTree `shouldBe` N (N (N L (2, 0) L) (1, 0) (N L (1, 1) L)) (0, 0) (N (N L (1, 1) L) (0, 1) (N L (0, 2) L))
  it "insert" $ do
    insert "b" L `shouldBe` N L "b" L
    insert "c" (N (N L "a" L) "b" L) `shouldBe` N (N L "a" L) "b" (N L "c" L)
    insert "a" (N (N L "b" L) "c" L) `shouldBe` N (N (N L "a" L) "b" L) "c" L
    insert "c" (N L "a" (N L "b" L)) `shouldBe` N L "a" (N L "b" (N L "c" L))
    insert "b" (N L "a" (N L "c" L)) `shouldBe` N L "a" (N (N L "b" L) "c" L)
  it "inOrder quickcheck" $
    inOrder (N L "a" L) `shouldBe` ["a"]
  it "inOrder Tree property test" $
    property inOrderTreeInsert

inOrderTreeInsert :: [Int] -> Property
inOrderTreeInsert xs = sort xs === xs'
  where
    xs' = inOrder $ foldr insert L xs

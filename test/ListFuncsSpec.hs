module ListFuncsSpec where

import Test.Hspec
import Prelude hiding (elem)

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++
elem :: (Eq a) => a -> [a] -> Bool
elem _ [] = False
elem a (x : xs) = a == x || elem a xs

-- Alternative with foldr. Perhaps too cryptic?
-- elem :: (Eq a) => a -> [a] -> Bool
-- elem a = foldr ((||) . (a ==)) False

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++
nub :: (Eq a) => [a] -> [a]
nub = foldr f []
  where
    f x xs = if elem x xs then xs else x : xs

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++
isAsc :: (Ord a) => [a] -> Bool
isAsc xs = and $ zipWith (<=) xs (tail xs)

-- Recursive version
-- isAsc :: (Ord a) => [a] -> Bool
-- isAsc [] = True
-- isAsc [_] = True
-- isAsc (x : y : xs) = x <= y && isAsc (y : xs)

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++
-- A function that determines whether a path from
-- 1 node to another exists within a directed graph
hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath [] _ _ = True
hasPath ns a b
  | a == b = True
  | otherwise =
    let (nexts, ps) = nextPaths
     in or $ fmap (\n -> hasPath ps n b) nexts
  where
    nextPaths = foldr f ([], []) ns
    f p@(a', b') (next, prune)
      | a == a' = (b' : next, prune)
      | otherwise = (next, p : prune)

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++
spec :: Spec
spec = describe "List funcs" $ do
  it "elem" $ do
    elem 'a' [] `shouldBe` False
    elem 'a' "aaaaa" `shouldBe` True
    elem 'g' "abcdefg" `shouldBe` True
    elem 'z' "abcdefg" `shouldBe` False
  it "nub" $ do
    nub ([] :: [Int]) `shouldBe` []
    nub "aaaaa" `shouldBe` "a"
    nub "hello" `shouldBe` "helo"
    nub "abcdefg" `shouldBe` "abcdefg"
  it "isAsc" $ do
    isAsc ([] :: [Int]) `shouldBe` True
    isAsc "a" `shouldBe` True
    isAsc "abbbcz" `shouldBe` True
    isAsc "aabba" `shouldBe` False
  it "hasPath" $ do
    hasPath [(1, 2)] 1 2 `shouldBe` True
    hasPath [(1, 5), (1, 2)] 1 5 `shouldBe` True
    hasPath [(1, 5), (5, 2)] 5 2 `shouldBe` True
    hasPath [(2, 1)] 1 2 `shouldBe` False
    hasPath [(1, 2), (2, 3)] 1 3 `shouldBe` True
    hasPath [(1, 2), (2, 3)] 3 1 `shouldBe` False
    hasPath [(1, 2), (2, 3), (3, 4)] 1 4 `shouldBe` True
    hasPath [(1, 2), (2, 3), (3, 4)] 4 3 `shouldBe` False

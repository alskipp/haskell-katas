module FoldFuncsSpec where

import Test.Hspec

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++
rev :: [a] -> [a]
rev = foldl (flip (:)) []

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++
prefixes :: [a] -> [[a]]
prefixes = foldr f []
  where
    f x xs = [x] : fmap (x :) xs

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++
data Trie a = Leaf a | Node a [Trie a]

foldtrie :: (b -> a -> b) -> b -> Trie a -> b
foldtrie f b (Leaf a) = f b a
foldtrie f b (Node a ts) = foldl (foldtrie f) (f b a) ts

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++
t0, t1, t2 :: Trie Int
t0 = Leaf 1
t1 = Node 1 [Leaf 1, Node 1 [Leaf 1]]
t2 = Node 0 [Leaf 0, Node 1 [Leaf 1]]

spec :: Spec
spec = describe "Fold funcs" $ do
  it "rev" $ do
    rev "" `shouldBe` ""
    rev "a" `shouldBe` "a"
    rev "abc" `shouldBe` "cba"
  it "prefixes" $ do
    prefixes "" `shouldBe` []
    prefixes "a" `shouldBe` ["a"]
    prefixes "abc" `shouldBe` ["a", "ab", "abc"]
  it "foldtrie" $ do
    foldtrie (+) 0 t0 `shouldBe` 1
    foldtrie (+) 0 t1 `shouldBe` 4
    foldtrie (+) 0 t2 `shouldBe` 2

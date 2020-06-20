module EditDistanceSpec where

import Test.Hspec

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++
distance :: Eq a => [a] -> [a] -> Int
distance = go 0
  where
    go n [] [] = n
    go n [] (_ : ys) = go (succ n) [] ys
    go n (_ : xs) [] = go (succ n) xs []
    go n (x : xs) (y : ys) = go (if x == y then n else succ n) xs ys

-- ++++++++++++++++++++++++++++++++++++++++++++++++++++
spec :: Spec
spec = describe "Edit distance" $ do
  it "plain distance" $ do
    distance "" "" `shouldBe` 0
    distance "a" "a" `shouldBe` 0
    distance "" "a" `shouldBe` 1
    distance "b" "a" `shouldBe` 1
    distance "abc" "acb" `shouldBe` 2
    distance "wibble" "wobble" `shouldBe` 1
    distance "hello" "hail" `shouldBe` 3

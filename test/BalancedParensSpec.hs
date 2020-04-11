module BalancedParensSpec where

import Test.Hspec
import Data.List ( sort )

balancedParens :: Int -> [String]
balancedParens 0 = [""]
balancedParens n = gen "" n 0

gen :: String -> Int -> Int -> [String]
gen s 0 r = [s ++ replicate r ')']
gen s n 0 = gen (s ++ "(") (n-1) 1
gen s n r = gen (s ++ "(") (n-1) (r+1) ++ gen (s ++ ")") n (r-1)


-- Write a function which makes a list of strings representing all of the ways you can balance n pairs of parentheses

spec :: Spec
spec = do
  describe "Basic Tests" $ do
    it "" $ do
      True `shouldBe` True
    it "n = 0" $ do
      (sort . balancedParens) 0 `shouldBe` [""]
    it "n = 1" $ do
      (sort . balancedParens) 1 `shouldBe` ["()"]
    it "n = 2" $ do
      (sort . balancedParens) 2 `shouldBe` ["(())","()()"]
    it "n = 3" $ do
      (sort . balancedParens) 3 `shouldBe` ["((()))","(()())","(())()","()(())","()()()"]
    it "n = 4" $ do
      (sort . balancedParens) 4 `shouldBe` ["(((())))","((()()))","((())())","((()))()","(()(()))","(()()())","(()())()","(())(())","(())()()","()((()))","()(()())","()(())()","()()(())","()()()()"]

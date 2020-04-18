module ScrambliesSpec where

import Data.List
  ( (\\),
    null,
  )
import Test.Hspec

scramble :: String -> String -> Bool
scramble s1 s2 = null $ s2 \\ s1

-- Complete the function scramble(str1, str2) that returns true if a portion of str1 characters can be rearranged to match str2, otherwise returns false.

spec :: Spec
spec =
  it "Scramble tests" $ do
    scramble "rkqodlw" "world" `shouldBe` True
    scramble "cedewaraaossoqqyt" "codewars" `shouldBe` True
    scramble "katas" "steak" `shouldBe` False
    scramble "scriptjavx" "javascript" `shouldBe` False
    scramble "scriptingjava" "javascript" `shouldBe` True
    scramble "scriptsjava" "javascripts" `shouldBe` True
    scramble "javscripts" "javascript" `shouldBe` False
    scramble "aabbcamaomsccdd" "commas" `shouldBe` True
    scramble "commas" "commas" `shouldBe` True
    scramble "sammoc" "commas" `shouldBe` True

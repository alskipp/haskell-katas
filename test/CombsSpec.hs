module CombsSpec where

import Data.List
  (
    tails,
  )
import Data.Maybe (catMaybes)
import Test.Hspec

combs :: String -> String -> Int
combs x y =
  (minimum . catMaybes) $
    fmap (combinedLength (length x) y) (tails x)
      ++ fmap
        (combinedLength (length y) x)
        (tails y)

combinedLength :: Int -> String -> String -> Maybe Int
combinedLength fullLenB a b = if combsFit then fullLength else Nothing
  where
    combsFit = and (zipWith isFit a b)
    isFit x y = x /= '*' || y /= '*'
    fullLength = Just $ lenA + fullLenB - min lenA lenB
    (lenA, lenB) = (length a, length b)

-- Miss X has only two combs in her possession, both of which are old and miss a tooth or two. She also has many purses of different length, in which she carries the combs. The only way they fit is horizontally and without overlapping. Given teeth' positions on both combs, find the minimum length of the purse she needs to take them with her.
-- It is guaranteed that there is at least one tooth at each end of the comb.
-- Note, that the combs can not be rotated/reversed.
-- A comb is represented as a string. If there is an asterisk ('*') in the ith position, there is a tooth there. Otherwise there is a dot ('.'), which means there is a missing tooth on the comb.

spec :: Spec
spec = it "example tests" $ do
  combs "*..*" "*.*" `shouldBe` 5
  combs "*...*" "*.*" `shouldBe` 5
  combs "*..*.*" "*.***" `shouldBe` 9
  combs "*.*" "*.*" `shouldBe` 4
  combs "*.**" "*.*" `shouldBe` 5
  combs "*.*" "**.*" `shouldBe` 5
  combs "*.**" "**..*.*" `shouldBe` 9
  combs "*.**.***" "*..**" `shouldBe` 13

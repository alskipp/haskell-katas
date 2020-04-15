module CapitalizeSpec where

import Data.Char (toUpper)
import Data.List (foldl')
import Test.Hspec

capitalize :: [Char] -> [String]
capitalize = fmap pToList $ snd . foldl' f ((toUpper, id), ([], []))
  where
    f ((g, h), (xs, ys)) y = ((h, g), (xs ++ [g y], ys ++ [h y]))
    pToList (x, y) = [x, y]

capitalize_v2 :: [Char] -> [String]
capitalize_v2 xs = [zipWith ($) fs xs, zipWith ($) (tail fs) xs]
  where
    fs = cycle [toUpper, id]

-- Given a string, capitalize the letters that occupy even indexes and odd indexes separately, and return as shown below. Index 0 will be considered even.
-- For example, capitalize("abcdef") = ['AbCdEf', 'aBcDeF']. See test cases for more examples.
-- The input will be a lowercase string with no spaces.

spec :: Spec
spec = do
  describe "Alternate capitalization" $ do
    it "should work for simple examples" $ do
      capitalize "abcdef" `shouldBe` ["AbCdEf", "aBcDeF"]
      capitalize "codewars" `shouldBe` ["CoDeWaRs", "cOdEwArS"]
      capitalize "abracadabra" `shouldBe` ["AbRaCaDaBrA", "aBrAcAdAbRa"]
      capitalize "codewarriors" `shouldBe` ["CoDeWaRrIoRs", "cOdEwArRiOrS"]
      capitalize "indexinglessons"
        `shouldBe` ["InDeXiNgLeSsOnS", "iNdExInGlEsSoNs"]
      capitalize "codingisafunactivity"
        `shouldBe` ["CoDiNgIsAfUnAcTiViTy", "cOdInGiSaFuNaCtIvItY"]

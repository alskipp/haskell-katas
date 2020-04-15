module DecodeMorseSpec where

import Data.List.Split (splitOn)
import Data.Map.Strict
  ( (!),
    Map,
  )
import qualified Data.Map.Strict as Map
import Test.Hspec

decodeMorse :: String -> String
decodeMorse =
  unwords . filter (/= []) . fmap (concatMap (morseCodes !) . words)
    . splitOn
      "   "

spec :: Spec
spec = do
  describe "decodeMorse" $ do
    it "should work on the example from the description" $ do
      decodeMorse ".... . -.--   .--- ..- -.. ." `shouldBe` "HEY JUDE"

morseCodes :: Map String String
morseCodes =
  Map.fromList
    [ (".-", "A"),
      ("-...", "B"),
      ("-.-.", "C"),
      ("-..", "D"),
      (".", "E"),
      ("..-.", "F"),
      ("--.", "G"),
      ("....", "H"),
      ("..", "I"),
      (".---", "J"),
      ("-.-", "K"),
      (".-..", "L"),
      ("--", "M"),
      ("-.", "N"),
      ("---", "O"),
      (".--.", "P"),
      ("--.-", "Q"),
      (".-.", "R"),
      ("...", "S"),
      ("-", "T"),
      ("..-", "U"),
      ("...-", "V"),
      (".--", "W"),
      ("-..-", "X"),
      ("-.--", "Y"),
      ("--..", "Z"),
      ("-----", "0"),
      (".----", "1"),
      ("..---", "2"),
      ("...--", "3"),
      ("....-", "4"),
      (".....", "5"),
      ("-....", "6"),
      ("--...", "7"),
      ("---..", "8"),
      ("----.", "9"),
      (".-.-.-", "."),
      ("--..--", ","),
      ("..--..", "?"),
      (".----.", "\""),
      ("-.-.--", "!"),
      ("-..-.", "/"),
      ("-.--.", "("),
      (",-.--.-", ")"),
      (".-...", "&"),
      ("---...", ""),
      ("-.-.-.", ";"),
      ("-...-", "="),
      (".-.-.", "+"),
      ("-....-", "-"),
      ("..--.-", "_"),
      (".-..-.", "\""),
      ("...-..-", "$"),
      (".--.-.", "@"),
      ("...---...", "SOS")
    ]

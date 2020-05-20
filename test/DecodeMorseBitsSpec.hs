module DecodeMorseBitsSpec where

import Data.List
  ( dropWhileEnd,
    group,
  )
import Data.List.Split
import Data.Map.Strict
  ( (!),
    Map,
  )
import qualified Data.Map.Strict as Map
import Data.Semigroup (Min (..))
import Test.Hspec

decodeBits :: String -> String
decodeBits bs =
  unwords
    . fmap
      ( concatMap (morseCodes !)
          . fmap (toSymbols r . splitOn bitSep)
          . splitOn charSep
      )
    . splitOn wordSep
    $ bs'
  where
    bs' = dropWhile (== '0') . dropWhileEnd (== '0') $ bs
    wordSep = replicate (r * 7) '0'
    charSep = replicate (r * 3) '0'
    bitSep = replicate r '0'
    r = rate bs'

toSymbols :: Int -> [String] -> String
toSymbols r = fmap (sign . length)
  where
    sign b = if (b `div` r) == 1 then '.' else '-'

rate :: String -> Int
rate = maybe 1 getMin . foldMap ((Just . Min) . length) . group

-- When transmitting the Morse code, the international standard specifies that:
--
-- "Dot" – is 1 time unit long.
-- "Dash" – is 3 time units long.
-- Pause between dots and dashes in a character – is 1 time unit long.
-- Pause between characters inside a word – is 3 time units long.
-- Pause between words – is 7 time units long.
-- However, the standard does not specify how long that "time unit" is. And in fact different operators would transmit at different speed. An amateur person may need a few seconds to transmit a single character, a skilled professional can transmit 60 words per minute, and robotic transmitters may go way faster.

spec :: Spec
spec = describe "Example tests" $ do
  it "should be HEY JUDE" $
    decodeBits
      "1100110011001100000011000000111111001100111111001111110000000000000011001111110011111100111111000000110011001111110000001111110011001100000011"
      `shouldBe` "HEY JUDE"
  it "should be MM" $ decodeBits "11101110001110111" `shouldBe` "MM"
  it "should be A" $ decodeBits "10111" `shouldBe` "A"
  it "should be E" $ decodeBits "01110" `shouldBe` "E"
  it "should be EE" $ decodeBits "10001" `shouldBe` "EE"
  it "should be EE" $ decodeBits "00010001" `shouldBe` "EE"
  it "should be EE" $ decodeBits "1000100000" `shouldBe` "EE"

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

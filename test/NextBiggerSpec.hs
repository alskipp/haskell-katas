module NextBiggerSpec where

import Data.Foldable
  (
    foldl',
  )
import Data.Semigroup (First (..))
import qualified Data.Sequence as S
import Data.Sequence
  ( (<|),
    (><),
    Seq (..),
  )
import Test.Hspec

nextBigger :: Int -> Maybe Int
nextBigger n = (fmap (fromDigits . getFirst) . foldMap f . S.reverse) zipped
  where
    zipped = let ds = toDigits n in S.zip (S.inits ds) (S.tails ds)
    f (h, t) = fmap (h ><) <$> maybeNext t

maybeNext :: Seq Int -> Maybe (First (Seq Int))
maybeNext Empty = Nothing
maybeNext (x :<| xs) = let s = S.sort xs in S.findIndexL (> x) s >>= f x s
  where
    f x' s i = fmap (First . (<| S.sort (S.update i x' s))) (S.lookup i s)

toDigits :: Int -> Seq Int
toDigits = S.unfoldl f
  where
    f n = if n == 0 then Nothing else Just $ quotRem n 10

fromDigits :: Seq Int -> Int
fromDigits = foldl' ((+) . (* 10)) 0

-- You have to create a function that takes a positive integer number and returns the next bigger number formed by the same digits:
-- 12 ==> 21
-- 513 ==> 531
-- 2017 ==> 2071

spec :: Spec
spec = it "example tests" $ do
  nextBigger 12 `shouldBe` Just 21
  nextBigger 513 `shouldBe` Just 531
  nextBigger 2017 `shouldBe` Just 2071
  nextBigger 414 `shouldBe` Just 441
  nextBigger 144 `shouldBe` Just 414
  nextBigger 1987654321 `shouldBe` Just 2113456789

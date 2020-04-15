module FirstNonConsecutiveSpec where

import Test.Hspec

firstNonConsecutive :: (Eq a, Enum a) => [a] -> Maybe a
firstNonConsecutive xs = f (Nothing, xs)
  where
    f (_, []) = Nothing
    f (Nothing, (a : as)) = f (Just a, as)
    f (Just x, (y : ys)) = if y == succ x then f (Just y, ys) else Just y

-- Your task is to find the first element of an array that is not consecutive.
-- By not consecutive we mean not exactly 1 larger than the previous element of the array.
-- E.g. If we have an array [1,2,3,4,6,7,8] then 1 then 2 then 3 then 4 are all consecutive but 6 is not, so that's the first non-consecutive number.
-- If the whole array is consecutive then return null2.

spec :: Spec
spec = do
  it "example tests" $ do
    firstNonConsecutive [1, 2, 3, 4, 6, 7, 8] `shouldBe` Just 6
    firstNonConsecutive [1, 2, 3] `shouldBe` Nothing

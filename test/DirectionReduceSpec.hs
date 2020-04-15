{-# LANGUAGE ViewPatterns #-}

module DirectionReduceSpec where

import Data.Foldable
  ( foldl',
    toList,
  )
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Data.Sequence
  ( Seq (..),
    ViewR ((:>)),
    (|>),
  )
import Test.Hspec
import Prelude hiding (last)

data Direction = North | East | West | South deriving (Eq, Show)

dirReduce :: [Direction] -> [Direction]
dirReduce = toList . foldl' f Seq.empty . Seq.fromList
  where
    f s x = if isOpposite s x then dropLast s else s |> x
    isOpposite s x = fromMaybe False $ fmap (op x) (last s)

last :: Seq a -> Maybe a
last (Seq.viewr -> _ :> x) = Just x
last (Seq.viewr -> _) = Nothing

dropLast :: Seq a -> Seq a
dropLast (Seq.viewr -> xs :> _) = xs
dropLast (Seq.viewr -> _) = Seq.empty

op :: Direction -> Direction -> Bool
op North South = True
op South North = True
op East West = True
op West East = True
op _ _ = False

spec :: Spec
spec = do
  describe "dirReduce - simple tests" $ do
    it "should work for some small examples" $ do
      dirReduce [] `shouldBe` []
      dirReduce [North] `shouldBe` [North]
      dirReduce [North, West] `shouldBe` [North, West]
      dirReduce [North, West, East] `shouldBe` [North]
      dirReduce [North, West, South, East] `shouldBe` [North, West, South, East]
      dirReduce [North, South, South, East, West, North, West] `shouldBe` [West]
      dirReduce [North, South, South, East, West, North] `shouldBe` []

module FoldMapSpec where

import Data.Foldable
  ( Foldable,
    foldMap,
  )
import Data.Maybe (isNothing)
import Data.Monoid ()
import Test.Hspec
import Test.QuickCheck

myToList :: Foldable t => t a -> [a]
myToList = foldMap (: [])

myMinimum :: (Ord a, Foldable t) => t a -> Maybe a
myMinimum = getMin . foldMap (Min . Just)

myFoldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
myFoldr f z t = getEndo (foldMap (Endo . f) t) z

newtype Endo a = Endo {getEndo :: a -> a}

instance Semigroup (Endo a) where
  (Endo a) <> (Endo b) = Endo (a . b)

instance Monoid (Endo a) where
  mempty = Endo id

newtype Min a = Min {getMin :: Maybe a} deriving (Show)

instance (Ord a) => Semigroup (Min a) where
  (Min (Just a)) <> (Min (Just b)) = Min $ Just (min a b)
  (Min Nothing) <> r = r
  l <> _ = l

instance (Ord a) => Monoid (Min a) where
  mempty = Min Nothing

-- The foldMap function can be used to implement all kind of folds. Here is its signature :

-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
-- In this exercise, you will have to implement the following functions, by increasing difficulty, in terms of foldMap :

-- myToList : turn any Foldable into a list
-- myMinimum : get the minimum value from any Foldable (hint : you will have to write a custom type, with a custom Monoid instance)
-- myFoldr : implement foldr in terms of foldMap (hint : there is a suitable Monoid in Data.Monoid)
-- There should be a single use of foldMap in each of the requested functions !

spec :: Spec
spec = describe "Testing Foldmap" $ do
  it "properly implements myToList" $
    property (\l -> myToList l == (l :: [Int]))
  it "properly implements myMinimum" $ property $ \l ->
    let r = myMinimum (l :: [Int])
     in if null l then isNothing r else r == Just (minimum l)
  it "properly implements foldr (and)" $
    property (\l -> myFoldr (&&) True l == and (l :: [Bool]))
  it "properly implements foldr (sum)" $
    property (\l -> myFoldr (+) 0 l == sum (l :: [Int]))
  it "properly implements foldr (++)" $
    property (\l -> myFoldr (++) [] l == concat (l :: [[Int]]))

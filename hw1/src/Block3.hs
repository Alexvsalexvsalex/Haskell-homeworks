module Block3
  (
    maybeConcat
  , NonEmpty(..)
  , ThisOrThat(..)
  ) where

import Data.Maybe

-- TASK 1
-- | Takes arrays from Just elements and concats all of them
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat xs = fromMaybe [] (mconcat xs)

-- | Representation of list containing at least 1 element
data NonEmpty a = a :| [a] deriving (Show)

instance Semigroup (NonEmpty t) where
  (a :| as) <> (b :| bs) = a :| (as <> [b] <> bs)

instance (Eq t) => Eq (NonEmpty t) where
  (a :| as) == (b :| bs) = (a == b) && (as == bs)

-- TASK 2
-- | Representation tuple-like structure that contains either one of
-- elements or both at the same time
data ThisOrThat a b = This a | That b | Both a b deriving (Show)

instance Semigroup (ThisOrThat t1 t2) where
  b@(Both _ _) <> _          = b
  (This l)     <> (This _)   = This l
  (This l)     <> (That r)   = Both l r
  (That r)     <> (This l)   = Both l r
  (That l)     <> (That _)   = That l
  (This l)     <> (Both _ r) = Both l r
  (That r)     <> (Both l _) = Both l r

instance (Eq t1, Eq t2) => Eq (ThisOrThat t1 t2) where
  (This l)     == (This r)     = l == r
  (That l)     == (That r)     = l == r
  (Both l1 l2) == (Both r1 r2) = l1 == r1 && l2 == r2
  _            == _            = False

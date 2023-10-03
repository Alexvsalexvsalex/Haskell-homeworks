{-# LANGUAGE InstanceSigs #-}

module Block2
  (
    splitOn
  , joinWith
  ) where

import qualified Data.List.NonEmpty as NE

import Block1(Tree(..))

-- TASK 1
instance Foldable Tree where
  foldr :: (t -> b -> b) -> b -> Tree t -> b
  foldr _    def Leaf          = def
  foldr func def (Node hs l r) = foldr func (foldr func (foldr func def r) hs) l

  foldMap :: Monoid m => (t -> m) -> Tree t -> m
  foldMap _    Leaf          = mempty
  foldMap func (Node hs l r) =
    foldMap func l `mappend` foldMap func hs `mappend` foldMap func r

-- TASK 2
-- | Splits NonEmpty list by given value to NonEmpty list of sublists
splitOn :: (Eq a) => a -> NE.NonEmpty a -> NE.NonEmpty [a]
splitOn splitElement xs =
  NE.fromList $
    foldr (\curElement (hSplitted : tSplitted) ->
        case splitElement == curElement of
          True -> [] : (hSplitted : tSplitted)
          _    -> (curElement : hSplitted) : tSplitted
      ) [[]] xs

-- | Joins NonEmpty list of lists with given element
joinWith :: (Eq a) => a -> NE.NonEmpty [a] -> NE.NonEmpty a
joinWith _           ([] NE.:| []) = error "result is empty list"
joinWith joinElement (h NE.:| hs)  =
  NE.fromList $
    h ++ foldr (\curElements joined -> joinElement : (curElements ++ joined)) [] hs

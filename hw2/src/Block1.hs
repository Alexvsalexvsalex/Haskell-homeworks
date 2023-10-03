module Block1
  (
    stringSum

  , Tree(..)

  , NonEmpty(..)
  ) where

-- TASK 1
-- | Takes string that contains numbers separated by whitespaces
-- | Returns sum of those numbers
stringSum :: String -> Maybe Int
stringSum = fmap sum . sequenceA . numbersList

numbersList :: String -> [Maybe Int]
numbersList = fmap (numbersListHelper 0) . words
  where
    numbersListHelper acc []        = Just acc
    numbersListHelper acc (c : str)
      | '0' <= c && c <= '9' =
        numbersListHelper (acc * 10 + fromEnum c - fromEnum '0') str
      | otherwise = Nothing

-- TASK 2
-- | Representation binary tree structure with values in leafs
data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a deriving (Show)

instance (Eq t) => Eq (Tree t) where
  (Leaf a)                 == (Leaf b)                 = a == b
  (Branch lChild1 rChild1) == (Branch lChild2 rChild2) = lChild1 == lChild2 && rChild1 == rChild2
  _                        == _                        = False

instance Functor Tree where
  fmap func (Leaf val)             = Leaf $ func val
  fmap func (Branch lChild rChild) = Branch (fmap func lChild) (fmap func rChild)

instance Applicative Tree where
  pure = Leaf
  (Branch lChild rChild) <*> valTree = Branch (lChild <*> valTree) (rChild <*> valTree)
  (Leaf func)            <*> valTree = fmap func valTree

instance Foldable Tree where
  foldMap func (Leaf val)             = func val
  foldMap func (Branch lChild rChild) = foldMap func lChild `mappend` foldMap func rChild

instance Traversable Tree where
  traverse func (Leaf val)             = pure Leaf <*> func val
  traverse func (Branch lChild rChild) = pure Branch <*> traverse func lChild <*> traverse func rChild

-- TASK 3
-- | Representation array with at least one element
data NonEmpty a = a :| [a] deriving (Show)

instance (Eq t) => Eq (NonEmpty t) where
  (a :| as) == (b :| bs) = a == b && as == bs

instance Functor NonEmpty where
  fmap func (x :| xs) = (func x) :| (fmap func xs)

instance Applicative NonEmpty where
  pure x = x :| []
  (f :| fs) <*> (x :| xs) = (f x) :| ((pure f <*> xs) ++ (fs <*> (x : xs)))

instance Monad NonEmpty where
  return x = x :| []
  xss >>= func = let r :| rs = fmap func xss in concatNonEmpty (r : rs)
                 where
                   concatNonEmpty [] = error "Unreachable code"
                   concatNonEmpty (a : []) = a
                   concatNonEmpty ((a :| as) : other) =
                     let b :| bs = concatNonEmpty other
                     in a :| (as ++ (b : bs))

instance Foldable NonEmpty where
  foldMap func (x :| xs) = func x `mappend` foldMap func xs

instance Traversable NonEmpty where
  traverse func (a :| as) = pure (:|) <*> func a <*> traverse func as

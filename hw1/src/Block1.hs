module Block1
  (
    Day(..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay

  , Nat(..)
  , isEven
  , toInteger'
  , quotRem'

  , Tree(..)
  , erase
  , eraseAll
  , find
  , fromList
  , isEmpty
  , insert
  , size
  , toList
  ) where

import qualified Data.List.NonEmpty as NE

-- TASK 1
-- | Representation of one day of week
data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show)

instance Enum Day where
  toEnum 0 = Monday
  toEnum 1 = Tuesday
  toEnum 2 = Wednesday
  toEnum 3 = Thursday
  toEnum 4 = Friday
  toEnum 5 = Saturday
  toEnum 6 = Sunday
  toEnum _ = undefined

  fromEnum Monday    = 0
  fromEnum Tuesday   = 1
  fromEnum Wednesday = 2
  fromEnum Thursday  = 3
  fromEnum Friday    = 4
  fromEnum Saturday  = 5
  fromEnum Sunday    = 6

instance Eq Day where
  d1 == d2 = fromEnum d1 == fromEnum d2

-- | Checks that the given is weekend(saturday or sunday)
isWeekend :: Day -> Bool
isWeekend = (> 4) . fromEnum

-- | Returns day that will be next to the given one
nextDay :: Day -> Day
nextDay = toEnum . (`mod` 7) . (+ 1) . fromEnum

-- | Returns day that was before the given one
afterDays :: Day -> Day
afterDays = toEnum . (`mod` 7) . (+ 6) . fromEnum

-- | Returns number of days between the given day and Friday
daysToParty :: Day -> Int
daysToParty = toEnum . (`mod` 7) . (11 -) . fromEnum

-- TASK 2
-- | Representation of natural number
data Nat = Z | S Nat deriving (Show)

instance Eq Nat where
  Z   == Z   = True
  S l == S r = l == r
  _   == _   = False

instance Ord Nat where
  Z   <= Z   = True
  Z   <= S _ = True
  S l <= S r = l <= r
  _   <= _   = False

instance Num Nat where
  l + Z     = l
  l + (S r) = S (l + r)
  _ * Z     = Z
  l * (S r) = (l * r) + l
  abs l    = l
  signum _ = S Z
  fromInteger a 
    | a > 0     = S (fromInteger (a - 1))
    | otherwise = Z
  l     - Z     = l
  (S l) - (S r) = l - r
  Z     - _     = Z

-- | Checks if number is divisible by 2
isEven :: Nat -> Bool
isEven nat = helper nat True
  where helper 0 evenFlag = evenFlag
        helper n evenFlag = helper (n - 1) (not evenFlag)

-- | Converts the given Nat to Integer
toInteger' :: Nat -> Integer
toInteger' Z     = 0
toInteger' (S n) = toInteger' n + 1

-- | Divide given Nat on another and returns tuple with quotient and remainder
quotRem' :: Nat -> Nat -> (Nat, Nat)
quotRem' a b
  | a >= b    = let (q, r) = quotRem' (a - b) b in (q + 1, r)
  | otherwise = (0, a)

-- TASK 3
-- | Representation of node in binary tree
data Tree a = Leaf | Node (NE.NonEmpty a) (Tree a) (Tree a) deriving (Show)

instance (Eq a) => Eq (Tree a) where
  Leaf            == Leaf            = True
  (Node a1 l1 r1) == (Node a2 l2 r2) = a1 == a2 && l1 == l2 && r1 == r2
  _               == _               = False

-- | Checks if the given tree contains any elements
isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

-- | Returns number of elements in the given tree
size :: Tree a -> Int
size Leaf         = 0
size (Node a l r) = size l + size r + length a

mapNode :: (Ord a) => (Tree a -> Tree a) -> Tree a -> Tree a -> a -> Tree a
mapNode _    def Leaf                        _ = def
mapNode func def n@(Node hs@(h NE.:| _) l r) x
  | h > x  = Node hs (mapNode func def l x) r
  | h == x = func n
  | h < x  = Node hs l (mapNode func def r x)

find :: (Ord a) => Tree a -> a -> Maybe (Tree a)
find Leaf                     _ = Nothing
find n@(Node (h NE.:| _) l r) x
  | h > x  = find l x
  | h == x = Just n
  | h < x  = find r x

-- | Adds the element to the binary tree
insert :: (Ord a) => Tree a -> a -> Tree a
insert tree x =
  mapNode (\(Node hs l r) ->
    Node (x NE.:| NE.toList hs) l r) (Node (x NE.:| []) Leaf Leaf) tree x

-- | Builds the binary tree from the given list with elements
fromList :: (Ord a) => [a] -> Tree a
fromList = foldl insert Leaf

-- | Returns sorted list with elements from tree
toList :: (Ord a) => Tree a -> [a]
toList Leaf          = []
toList (Node hs l r) = toList l ++ NE.toList hs ++ toList r

getMax :: (Ord a) => Tree a -> Tree a
getMax Leaf              = Leaf
getMax n@(Node _ _ Leaf) = n
getMax (Node _ _ r)      = getMax r

-- | Removes one element with the given value from tree
erase :: (Ord a) => Tree a -> a -> Tree a
erase = mapNode (\n@(Node (_ NE.:| hs) _ _) -> fixAfterErase n hs) Leaf

-- | Removes all elements with the given value from tree
eraseAll :: (Ord a) => Tree a -> a -> Tree a
eraseAll = mapNode (`fixAfterErase` []) Leaf

getMaxArray :: (Ord a) => Tree a -> Maybe (NE.NonEmpty a)
getMaxArray tree =
  case getMax tree of
    Leaf        -> Nothing
    Node hs _ _ -> Just hs

fixAfterErase :: (Ord a) => Tree a -> [a] -> Tree a
fixAfterErase Leaf             _        = Leaf
fixAfterErase (Node _  Leaf r) []       = r
fixAfterErase (Node _ l r)     (h : hs) = Node (h NE.:| hs) l r
fixAfterErase (Node _ l r)     []       =
  case getMaxArray l of
    Just as@(a  NE.:| _) -> Node as (eraseAll l a) r
    Nothing              -> Leaf

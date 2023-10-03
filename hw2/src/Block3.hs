module Block3
  (
    Parser(..)

  , element
  , eof
  , ok
  , satisfy
  , stream

  , bracketsParser
  , numberParser

  , listlistParser
  ) where

import Control.Applicative

-- TASK 1
-- | Structure that takes raw data stream and transform to specified data type
data Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap f lazyP = Parser $ fmap (\(a, b) -> (f a, b)) . (runParser lazyP)

instance Applicative (Parser s) where
  pure a = Parser $ \s -> pure (a, s)
  lazyP1 <*> lazyP2 = Parser $
    \s -> do
      (f, t) <- runParser lazyP1 $ s
      (a, r) <- runParser lazyP2 $ t
      return (f a, r)

instance Monad (Parser s) where
  return = pure
  lazyP >>= f = Parser $
    \s -> do
      (x, l) <- runParser lazyP $ s
      runParser (f x) $ l

instance Alternative (Parser s) where
  empty = Parser $ \_ -> Nothing
  lazyP1 <|> lazyP2 = Parser $ \s -> (runParser lazyP1 $ s) <|> (runParser lazyP2 $ s)

-- TASK 2
-- | Doesn't fail always and doesn't read input
ok :: Parser s ()
ok = return ()

-- | Doesn't fail only when input is empty
eof :: Parser s ()
eof = Parser $ \s -> if null s then Just ((), []) else Nothing

-- | Takes symbol from input and fails if it doesn't satisfy given predicate
satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser $ \s -> case s of
    []     -> Nothing
    (x:xs) -> if p x then Just (x, xs) else Nothing

getElements :: Int -> Parser s [s]
getElements e = Parser $ 
  \s -> let (l, r) = splitAt e s in
    if length l == e then Just (l, r) else Nothing

getElement :: Parser s s
getElement = fmap head $ getElements 1

-- | Takes symbol from input and fails if it doesn't equal to given one
element :: Eq s => s -> Parser s s
element c = satisfy (== c)

-- | Takes sequence from input and fails if it doesn't equal to given one
stream :: Eq s => [s] -> Parser s [s]
stream []     = return []
stream (x:xs) = mappend <$> (fmap return $ element x) <*> stream xs

-- TASK 3
-- | Parser processing string with brackets
-- | Doesn't fail only when bracket's sequence if correct
bracketsParser :: Parser Char ()
bracketsParser = bracketsOrEmpty *> eof
  where
    bracketsOrEmpty = (element '(' *> bracketsOrEmpty *> element ')' *> bracketsOrEmpty) <|> ok

sign :: Parser Char String
sign = stream "-" <|> (stream "+" *> stream "") <|> stream ""

-- | Parser processing string with single number
-- | Either returns Int that was in the string or fails
numberParser :: Parser Char Int
numberParser = read <$> (mappend <$> sign <*> (some $ satisfy (\d -> '0' <= d && d <= '9')))

-- TASK 4
skipSpaces :: Parser Char ()
skipSpaces = (many $ element ' ') *> ok

numberListParser :: Parser Char [Int]
numberListParser =
  (many $ (skipSpaces *> numberParser <* skipSpaces <* (eof <|> stream "," *> ok))) <* eof

singleGroupInt :: Parser Int [Int]
singleGroupInt = do
  k <- getElement
  getElements k

groupInts :: Parser Int [[Int]]
groupInts = (many singleGroupInt) <* eof

redirectParser :: Parser a [b] -> Parser b c -> Parser a c
redirectParser p1 p2 = Parser $
  \s -> do
    (list, left) <- runParser p1 s
    (answer, _)  <- runParser p2 list
    return (answer, left)

-- | Parse list of lists of Int from the string
-- | Format is "k1, num_1_1, num_1_2, ..., num_1_k1, k2, num_2_1..."
-- | k1, k2... - lengths of lists, num_i_j - data
listlistParser :: Parser Char [[Int]]
listlistParser = redirectParser numberListParser groupInts

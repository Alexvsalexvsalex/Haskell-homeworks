module Block2
  (
    ArithmeticError(..)
  , Expr(..)
  , eval

  , moving
  ) where

import Control.Monad.State

-- TASK 1
-- | Parse tree of arithmetic expression
data Expr = Const Int | Expr :+ Expr | Expr :- Expr |
  Expr :* Expr | Expr :/ Expr | Expr :^ Expr deriving (Show)

newtype ArithmeticError = ArithmeticError String deriving (Eq, Show)

unSafeEval ::
  Expr -> Expr -> (Int -> Int -> Int) -> (Int -> Int -> Bool) -> String ->
    Either ArithmeticError Int
unSafeEval l r op predicate errorMessage = do
  lRes <- eval l
  rRes <- eval r
  if predicate lRes rRes
  then return $ lRes `op` rRes
  else Left $ ArithmeticError errorMessage

safeEvalImpl :: Expr -> Expr -> (Int -> Int -> Int) -> Either ArithmeticError Int
safeEvalImpl l r op = unSafeEval l r op (\_ _ -> True) "Unreachable section"

-- | Evaluate expression
-- | Returns either error in Left or result in Right
eval :: Expr -> Either ArithmeticError Int
eval (Const c) = Right c
eval (l :+ r) = safeEvalImpl l r (+)
eval (l :- r) = safeEvalImpl l r (-)
eval (l :* r) = safeEvalImpl l r (*)
eval (l :/ r) = unSafeEval l r div (\_ s -> s /= 0) "Division by zero"
eval (l :^ r) = unSafeEval l r (^) (\_ s -> s >= 0) "Negative raising"

-- TASK 2
-- | Queue based on two lists
-- | Contains two lists, length
data Queue e = Queue [e] [e] Int

emptyQ :: Queue a
emptyQ = Queue [] [] 0

pushQ :: Queue a -> a -> Queue a
pushQ (Queue forDe forEn l) x = Queue forDe (x : forEn) (l + 1)

pushWithLimitQ :: Queue a -> Int -> a -> ([a], Queue a)
pushWithLimitQ queue limit x =
  let (multipopped, nQueue) = multipopQ queue (max 0 ((lenQ queue) - limit + 1))
  in (multipopped, pushQ nQueue x)

multipopQ :: Queue a -> Int -> ([a], Queue a)
multipopQ queue 0 = ([], queue)
multipopQ queue n =
  let (popped     , nQueue)  = popQ queue
      (multipopped, nQueue2) = multipopQ nQueue (n - 1)
      in (popped : multipopped, nQueue2)

popQ :: Queue a -> (a, Queue a)
popQ (Queue []       []    _) = error "Pop from empty queue"
popQ (Queue []       forEn l) = popQ (Queue (reverse forEn) [] l)
popQ (Queue (x : xs) forEn l) = (x, Queue xs forEn (l - 1))

lenQ :: Queue a -> Int
lenQ (Queue _ _ l) = l

-- | Specified queue with Ints
-- | Additionally contains sum of elements
data QueueInt = QueueInt (Queue Int) Int

emptyQI :: QueueInt
emptyQI = QueueInt emptyQ 0

pushWithLimitQI :: QueueInt -> Int -> Int -> QueueInt
pushWithLimitQI (QueueInt queue curSum) limit x =
  let (popped, nQueue) = pushWithLimitQ queue limit x
  in QueueInt (nQueue) (curSum + x - (sum popped))

getAverageQI :: QueueInt -> Float
getAverageQI (QueueInt queue curSum) =
  (fromIntegral curSum) / (fromIntegral $ lenQ queue)

-- | State for queue of Ints
-- | Contains queue, window size, averages
type SumQueueState = State (QueueInt, Int, [Float])

pushS :: Int -> SumQueueState ()
pushS x = state $
  \(queue, limit, averages) -> ((),
    let nQueue = pushWithLimitQI queue limit x
    in (nQueue, limit, (getAverageQI nQueue) : averages)
  )

movingImpl :: [Int] -> SumQueueState [Float]
movingImpl []       = do
  (_, _, averages) <- get
  return . reverse $ averages
movingImpl (x : xs) = do
  pushS x
  movingImpl xs

-- | Calculates simple moving average by window's size and list of Ints
moving :: Int -> [Int] -> [Float]
moving n list
  | n > 0 = evalState (movingImpl list) (emptyQI, n, [])
  | otherwise = error "Negative window's size"

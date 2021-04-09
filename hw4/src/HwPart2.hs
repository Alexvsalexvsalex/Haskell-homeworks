{-# LANGUAGE FlexibleInstances #-}

module HwPart2
  (
    createCovid
  , evolveCovid
  ) where

import Control.Comonad
  (
    Comonad(..))

import System.Random
  (
    StdGen
  , mkStdGen
  , randomR)

data ListZipper a = LZ [a] a [a]

listLeft, listRight :: ListZipper a -> ListZipper a
listLeft (LZ (a:as) x bs) = LZ as a (x:bs)
listLeft _ = error "listLeft"

listRight (LZ as x (b:bs)) = LZ (x:as) b bs
listRight _ = error "listRight"

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ ls _ rs) = LZ ls x rs

toList :: ListZipper a -> Int -> [a]
toList (LZ ls x rs) n = reverse (take n ls) ++ [x] ++ take n rs

instance Functor ListZipper where
  fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

instance Comonad ListZipper where
  extract (LZ _ x _) = x

  duplicate = mkZipper listLeft listRight

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

mkZipper :: (v -> v) -> (v -> v) -> v -> ListZipper v
mkZipper genLeft genRight e =
  LZ (iterateTail genLeft e) e (iterateTail genRight e)

newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

instance Show (Grid Cell) where
  show g = concat . map (\r -> foldMap show r ++ "\n") $ toMatrix 4 4 g

instance Functor Grid where
  fmap f (Grid g) = Grid $ fmap (fmap f) g

toMatrix :: Int -> Int -> Grid a -> [[a]]
toMatrix h w (Grid g) = toList (fmap (\i -> toList i w) g) h

up, down :: Grid a -> Grid a
up   (Grid g) = Grid (listLeft  g)
down (Grid g) = Grid (listRight g)

left, right :: Grid a -> Grid a
left  (Grid g) = Grid (fmap listLeft  g)
right (Grid g) = Grid (fmap listRight g)

gridRead :: Grid a -> a
gridRead (Grid g) = extract $ extract g

gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ listWrite newLine g
  where
    oldLine = extract g
    newLine = listWrite x oldLine

horizontal, vertical :: Grid a -> ListZipper (Grid a)
horizontal = mkZipper left right
vertical   = mkZipper up   down

instance Comonad Grid where
  extract = gridRead

  duplicate = Grid . fmap horizontal . vertical

type InfectionProbability = Float
type IncubationTime = Int
type IllTime = Int
type ImmunityTime = Int

data CellState
  = Empty
  | Incubation IncubationTime
  | Ill IllTime
  | Immunity ImmunityTime

data Cell = Cell CellState StdGen

instance Show Cell where
  show (Cell st _) = case st of
    Empty -> " "
    Incubation _ -> "#"
    Ill _ -> "#"
    Immunity _ -> "@"

-- Transform cell's state
applyToCellState :: (CellState -> CellState) -> Cell -> Cell
applyToCellState func (Cell cS g) = Cell (func cS) g

type Covid19Settings
  = (InfectionProbability, IncubationTime, IllTime, ImmunityTime)

data Covid19 = Covid19 (Grid Cell) Covid19Settings

instance Show Covid19 where
  show (Covid19 grid settings) =
    "Covid simulation: " ++ (show settings) ++ "\n" ++ (show grid)

-- Transform grid and correct result
applyToCovidGrid :: (Grid Cell -> Grid Cell) -> Covid19 -> Covid19
applyToCovidGrid func (Covid19 grid s) = Covid19 (fmap (correct s) $ func grid) s

neighbours :: [Grid a -> Grid a]
neighbours = [left, right, up, down]

-- Ability to infect
isIll :: Cell -> Bool
isIll (Cell (Incubation _) _) = True
isIll (Cell (Ill _) _) = True
isIll _ = False

-- Getting ill status of neighbours
neighboursState :: Grid Cell -> [Bool]
neighboursState g = map (\direction -> isIll . extract $ direction g) neighbours

-- Correcting cell's state, ending of period
-- Recursive call of helper needs when time of some period is zero
correct :: Covid19Settings -> Cell -> Cell
correct settings = applyToCellState (helper settings)
  where
    helper s@(_, iT, _, _) (Incubation incT)
      | incT == iT = helper s (Ill 0)
    helper s@(_, _, iT, _) (Ill        illT)
      | illT == iT = helper s (Immunity 0)
    helper   (_, _, _, iT) (Immunity   immT)
      | immT == iT = Empty
    helper _ obj = obj

-- Increasing counters of cells
daySkip :: Covid19 -> Covid19
daySkip
  = applyToCovidGrid (fmap (applyToCellState increaseTime))
  where
    increaseTime Empty = Empty
    increaseTime (Incubation incTime) = Incubation (incTime + 1)
    increaseTime (Ill illTime) = Ill (illTime + 1)
    increaseTime (Immunity immTime) = Immunity (immTime + 1)

-- Getting random probabilities
getRandProbs :: StdGen -> Int -> ([Float], StdGen)
getRandProbs stdGen 0   = ([], stdGen)
getRandProbs stdGen cnt =
  let (resL, resGen)  = getRandProbs stdGen (cnt - 1)
      (randNum, nGen) = randomR (0 :: Float, 1 :: Float) resGen
  in ((randNum:resL), nGen)

-- Infect neighbours
infection :: Covid19 -> Covid19
infection covid@(Covid19 _ (critProb, _, _, _))
  = applyToCovidGrid (extend transferInfection) covid
  where
    transferInfection grid =
      let c@(Cell tp gen) = extract grid
      in case tp of
        Empty ->
          let (randNums, newGen) = getRandProbs gen 4
          in
            if any (\(p1, p2) -> p1 && p2)
              (zip (map (<critProb) randNums) (neighboursState grid))
            then Cell (Incubation 0) newGen
            else Cell tp newGen
        _ -> c

-- Step of simulation
evolveCovid :: Covid19 -> Covid19
evolveCovid = infection . daySkip

-- World without infection
emptyWorld :: Grid Cell
emptyWorld = Grid .
  duplicate .
    fmap (\i -> Cell Empty (mkStdGen i)) $ mkZipper (subtract 1) (+1) 0

-- Completed world with one infection
createCovid ::
  InfectionProbability -> IncubationTime -> IllTime -> ImmunityTime
    -> Covid19
createCovid prob incT illT immT =
  applyToCovidGrid
    (\g -> gridWrite (applyToCellState (\_ -> Incubation 0) $ extract g) g) $
      Covid19 emptyWorld (prob, incT, illT, immT)

main :: IO ()
main = do
  let world = createCovid 0.75 1 1 1
  putStrLn (show (iterate evolveCovid world !! 2))
  putStrLn (show (iterate evolveCovid world !! 5))
  putStrLn (show (iterate evolveCovid world !! 10))
  putStrLn (show (iterate evolveCovid world !! 30))
  putStrLn (show (iterate evolveCovid world !! 50))

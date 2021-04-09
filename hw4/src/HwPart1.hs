{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module HwPart1
  (
    Point(..)
  , crossProduct
  , doubleArea
  , minus
  , perimeter
  , plus
  , scalarProduct

  , parMonteCarlo
  , seqMonteCarlo

  , FS(..)
  , cd
  , contentsL
  , file
  , ls
  , nameL
  ) where

import Lens.Micro
  (
    Lens'
  , Traversal'
  , filtered
  , lens
  , traversed)

import Data.List
  (
    foldl')

import System.Random
  (
    mkStdGen
  , randomRs)

import Control.Monad.Par.Combinator
  (
    parMap)

import Control.Monad.Par
  (
    runPar)

-- Task 1
data Point = Point !Int !Int deriving (Eq, Show)

plus :: Point -> Point -> Point
plus (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

minus :: Point -> Point -> Point
minus (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

scalarProduct :: Point -> Point -> Int
scalarProduct (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

crossProduct  :: Point -> Point -> Int
crossProduct (Point x1 y1) (Point x2 y2) = x1 * y2 - x2 * y1

vectorLen' :: Point -> Double
vectorLen' p = sqrt . fromIntegral $! scalarProduct p p

makeListPairs :: [Point] -> [(Point, Point)]
makeListPairs xs = zip (last xs:xs) xs

sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

perimeter :: [Point] -> Double
perimeter = sum' . map (vectorLen' . uncurry minus) . makeListPairs

doubleArea :: [Point] -> Int
doubleArea = sum' . map (uncurry crossProduct) . makeListPairs

-- Task 2
seqMonteCarloImpl :: Int -> (Float -> Float) -> (Float, Float) -> Int -> Float
seqMonteCarloImpl salt func rng tries =
  correctMul * (sum' . map func . take tries . randomRs rng $ mkStdGen salt)
  where
    correctMul = (snd rng - fst rng) / (fromIntegral tries)

seqMonteCarlo :: (Float -> Float) -> (Float, Float) -> Int -> Float
seqMonteCarlo = seqMonteCarloImpl 0

parMonteCarlo :: Int -> (Float -> Float) -> (Float, Float) -> Int -> Float
parMonteCarlo threads func rng tries =
  (sum' . runPar $ parMap (\i -> seqMonteCarloImpl i func rng tries) [0..threads-1]) / (fromIntegral threads)

-- Task 6 and 7
data FS
    = Dir
    { name     :: FilePath  -- название папки, не полный путь
    , contents :: [FS]
    }
    | File
    { name     :: FilePath  -- название файла, не полный путь
    }

nameL :: Lens' FS FilePath
nameL = lens name (\fs v -> fs { name = v })

contentsL :: Lens' FS [FS]
contentsL = lens contents (\fs v -> fs { contents = v })

getContentByName :: Int -> FilePath -> Traversal' FS FS
getContentByName tp fN =
  contentsL . traversed
    . filtered (\fs -> case fs of
        File nm  -> tp == 0 && nm == fN
        Dir nm _ -> tp == 1 && nm == fN)

cd :: FilePath -> Traversal' FS FS
cd fN = getContentByName 1 fN

ls :: Traversal' FS FilePath
ls = contentsL . traversed . nameL

file :: FilePath -> Traversal' FS FilePath
file fN = getContentByName 0 fN . nameL

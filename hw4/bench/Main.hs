module Main
  (
    main
  ) where

import Criterion.Main
  (
    bench
  , bgroup
  , defaultMain
  , nf)

import HwPart1
  (
    Point(..)
  , doubleArea
  , perimeter

  , parMonteCarlo
  , seqMonteCarlo)

geomLim :: Int
geomLim = 2500000

genPoints :: (Int -> Point) -> [Point]
genPoints f = [f a | a <- [0..geomLim]]

bigSquare :: [Point]
bigSquare
  =  genPoints (\i -> Point i 0)
  ++ genPoints (\i -> Point geomLim i)
  ++ genPoints (\i -> Point (geomLim - i) geomLim)
  ++ genPoints (\i -> Point 0 (geomLim - i))

compressedBigSquare :: [Point]
compressedBigSquare =
  map (\(Point x y) -> Point (x `div` 100) (y `div` 100)) bigSquare

mCTest ::
  ((Float -> Float) -> (Float, Float) -> Int -> Float) -> (Int -> Float)
mCTest monteCarloImpl = monteCarloImpl (sin . cos) (0, pi/2)

main :: IO ()
main = defaultMain [
    bgroup "geometry"
      [ bench "perimeter"  $ nf perimeter compressedBigSquare
      , bench "doubleArea" $ nf doubleArea compressedBigSquare
      ]
  , bgroup "monte carlo"
      [ bench "seq 10k_tries" $ nf (mCTest seqMonteCarlo) 10000
      , bench "seq 1m_tries" $ nf (mCTest seqMonteCarlo) 1000000
      , bench "par 1th 10k_tries" $ nf (mCTest $ parMonteCarlo 1) 10000
      , bench "par 1th 1m_tries" $ nf (mCTest $ parMonteCarlo 1) 1000000
      , bench "par 4ths 10k_tries" $ nf (mCTest $ parMonteCarlo 4) (10000 `div` 4)
      , bench "par 4ths 1m_tries" $ nf (mCTest $ parMonteCarlo 4) (1000000 `div` 4)
      ]
  ]

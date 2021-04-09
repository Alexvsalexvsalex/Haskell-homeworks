module TasksSpec
  (
    spec
  ) where

import Test.Hspec
  (
    Spec
  , it
  , shouldBe)

import Test.Hspec.Expectations
  (
    Expectation)

import Lens.Micro
  (
    (^?)
  , (^..))

import HwPart1
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
  , file
  , ls)

import HwPart2
  (
    createCovid
  , evolveCovid)

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

shouldBeClose :: (Ord a, Fractional a) => a -> a -> Expectation
actual `shouldBeClose` expected = (actual - expected < 1e-3) `shouldBe` True

easyFunc, mediumFunc, hardFunc :: Float -> Float
easyFunc = id
mediumFunc = sin
hardFunc x = 1.0 / tan(x ** 2) - cos(x)

countLetter :: Char -> String -> Int
countLetter c = length . filter (== c)

spec :: Spec
spec = do
  it "binary operations" $ do
    shouldBe (Point 1 1 `plus` Point 3 2) (Point 4 3)
    shouldBe (Point 3 2 `minus` Point 1 1) (Point 2 1)
    shouldBe (Point 1 1 `scalarProduct` Point 3 2) 5
    shouldBe (Point 1 1 `crossProduct` Point 3 2) (-1)

  it "polygon process" $ do
    let square = [Point 0 0, Point 1 0, Point 1 1, Point 0 1]

    shouldBe (perimeter square) 4
    shouldBe (doubleArea square) 2

    shouldBeClose (perimeter bigSquare) (fromIntegral $ 4 * geomLim)

    shouldBeClose (perimeter compressedBigSquare) (fromIntegral $ 4 * geomLim `div` 100)
    shouldBe (doubleArea compressedBigSquare) (2 * (geomLim `div` 100) * (geomLim `div` 100))

  it "monte carlo seq" $ do
    shouldBeClose (seqMonteCarlo easyFunc (0, 10) 5000000) 50
    shouldBeClose (seqMonteCarlo mediumFunc (0, pi/2) 5000000) (pi/2)
    shouldBeClose (seqMonteCarlo hardFunc (pi/4, 1) 5000000) 0.078811333897

  it "monte carlo par" $ do
    shouldBeClose (parMonteCarlo 4 easyFunc (0, 10) 1000000) 50
    shouldBeClose (parMonteCarlo 4 mediumFunc (0, pi/2) 1000000) (pi/2)
    shouldBeClose (parMonteCarlo 4 hardFunc (pi/4, 1) 1000000) 0.078811333897

  it "FS lenses" $ do
    let testFS1 = Dir {
        name = ""
      , contents = [
            File { name = "abc" }
          , Dir {
              name = "dir"
            , contents = [
                File { name = "file" }
            ]}
          ]}

    shouldBe (testFS1 ^?  cd "dir" . file "C") Nothing
    shouldBe (testFS1 ^?  cd "dir" . file "file") (Just "file")
    shouldBe (testFS1 ^?  file "abc") (Just "abc")
    shouldBe (testFS1 ^?  file "abcd") Nothing
    shouldBe (testFS1 ^.. ls) ["abc", "dir"]
    shouldBe (testFS1 ^.. cd "dir" . ls) ["file"]

  it "covid" $ do
    let world = createCovid 0.65 1 1 1
    shouldBe (countLetter '#' . show $ iterate evolveCovid world !! 0) 1
    shouldBe (countLetter '@' . show $ iterate evolveCovid world !! 0) 0
    shouldBe (countLetter '#' . show $ iterate evolveCovid world !! 5) 7
    shouldBe (countLetter '@' . show $ iterate evolveCovid world !! 5) 2
    shouldBe (countLetter '#' . show $ iterate evolveCovid world !! 10) 21
    shouldBe (countLetter '@' . show $ iterate evolveCovid world !! 10) 13

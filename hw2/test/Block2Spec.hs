module Block2Spec
  (
    spec
  ) where

import Test.Hspec

import Block2

spec :: Spec
spec = do
  let sampleSum = (Const 2) :+ (Const 2)
  let sampleSub = (Const 2) :- (Const 2)
  let sampleMul = (Const 3) :* sampleSum
  let divByZero = sampleSum :/ sampleSub
  let sampleDiv = sampleMul :/ sampleSum
  let samplePow = sampleSum :^ sampleSum
  let negPowErr = sampleMul :^ (Const (-1))
  it "correctExpressions" $ do
    shouldBe (eval sampleSum) (Right 4)
    shouldBe (eval sampleSub) (Right 0)
    shouldBe (eval sampleMul) (Right 12)
    shouldBe (eval sampleDiv) (Right 3)
    shouldBe (eval samplePow) (Right 256)

  it "incorrectExpressions" $ do
    shouldBe (eval divByZero) (Left $ ArithmeticError "Division by zero")
    shouldBe (eval negPowErr) (Left $ ArithmeticError "Negative raising")

  it "movingAverage" $ do
    shouldBe (moving 2 [1, 5, 3, 8, 7, 9, 6])
             [1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]
    shouldBe (moving 4 [1, 5, 3, 8, 7, 9, 6])
             [1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]
    shouldBe (moving 1 [1, 5, 3, 8, 7, 9, 6])
             [1.0, 5.0, 3.0, 8.0, 7.0, 9.0, 6.0]


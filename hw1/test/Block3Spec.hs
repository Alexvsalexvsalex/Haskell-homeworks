module Block3Spec
  (
    spec
  ) where

import Test.Hspec

import Block3

spec :: Spec
spec = do
  it "maybeConcat" $ do
    shouldBe (maybeConcat [Just [1,2,3], Nothing, Just [4,5]]) [1,2,3,4,5]
    shouldBe (maybeConcat ([Nothing, Nothing, Nothing] :: [Maybe [Int]])) []
    shouldBe (maybeConcat [Just [1, 1, 1]]) [1, 1, 1]

  it "Semigroup for NonEmpty" $ do
    shouldBe ((1 :| [2, 3]) <> (4 :| [5, 6])) (1 :| [2, 3, 4, 5, 6])
    shouldBe ((1 :| []) <> (2 :| []) <> (3 :| [])) (1 :| [2, 3])
    shouldBe ((1 :| []) <> (2 :| []) <> (3 :| [])) (1 :| [2, 3])

  it "Semigroup for ThisOrThat" $ do
    shouldBe (This 'a' <> That 'b') (Both 'a' 'b')
    shouldBe (This 1 <> This 5 :: ThisOrThat Int Int) (This 1)
    shouldBe (Both 0 0 <> This 5) (Both 0 0)

module Block1Spec
  (
    spec
  ) where

import Test.Hspec
import Data.Functor.Identity
import Data.Functor.Compose

import Block1

spec :: Spec
spec = do
  it "stringSumCorrectStrings" $ do
    shouldBe (stringSum "  1 3  5 0 7 0") (Just 16)
    shouldBe (stringSum "2 2") (Just 4)
    shouldBe (stringSum "321 123") (Just 444)

  it "stringSumIncorrectStrings" $ do
    shouldBe (stringSum "  1 3a  5 0 7 0") Nothing
    shouldBe (stringSum "two two") Nothing
    shouldBe (stringSum "321 I23") Nothing

  let sampleFunctionTree1 = Branch (Branch (Leaf (+1)) (Leaf (*2))) (Leaf (+(-2)))
  let sampleFunctionTree2 = Branch (Leaf (*3)) (Leaf (+10))
  let sampleValueTree1 = Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3)
  let sampleValueTree2 = Branch (Branch (Leaf 7) (Leaf 8)) (Leaf 9)
  let sampleFunction = (+4)
  let sampleValue = 12
  it "instanceFunctorTree" $ do
    shouldBe (fmap id sampleValueTree1) sampleValueTree1
    shouldBe (fmap (+5) . fmap (*2) $ sampleValueTree1) (fmap ((+5) . (*2)) sampleValueTree1)
    shouldBe (fmap (+6) sampleValueTree1) sampleValueTree2

  it "instanceApplicativeTree" $ do
    shouldBe (pure id <*> sampleValueTree1) sampleValueTree1
    shouldBe (pure (.) <*> sampleFunctionTree1 <*> sampleFunctionTree2 <*> sampleValueTree1)
             (sampleFunctionTree1 <*> (sampleFunctionTree2 <*> sampleValueTree1))
    shouldBe (pure (.) <*> sampleFunctionTree1 <*> sampleFunctionTree2 <*> sampleValueTree2)
             (sampleFunctionTree1 <*> (sampleFunctionTree2 <*> sampleValueTree2))
    shouldBe ((pure sampleFunction :: Tree (Integer -> Integer)) <*> pure sampleValue)
             (pure (sampleFunction sampleValue))
    shouldBe (sampleFunctionTree1 <*> pure sampleValue)
             (pure ($ sampleValue) <*> sampleFunctionTree1)

  it "instanceFoldableTree" $ do
    shouldBe (foldl (+) 0 sampleValueTree1) 6
    shouldBe (foldr (:) [] sampleValueTree2) [7, 8, 9]

  it "instanceTraversableTree" $ do
    shouldBe (traverse Identity sampleValueTree1) (Identity sampleValueTree1)
    shouldBe (traverse Just sampleValueTree1) (Just sampleValueTree1)

  let sampleFunctionNonEmpty1 = (+1) :| [(+4)]
  let sampleFunctionNonEmpty2 = (*2) :| [(*7)]
  let sampleValueNonEmpty1 = 1 :| [2, 3]
  let sampleValueNonEmpty2 = 4 :| [5, 6]
  let sampleFunction2 = (*3)
  it "instanceFunctorNonEmpty" $ do
    shouldBe (fmap id sampleValueNonEmpty1) sampleValueNonEmpty1
    shouldBe (fmap (+5) . fmap (*2) $ sampleValueNonEmpty1)
             (fmap ((+5) . (*2)) sampleValueNonEmpty1)
    shouldBe (fmap (+3) sampleValueNonEmpty1) sampleValueNonEmpty2

  it "instanceApplicativeNonEmpty" $ do
    shouldBe (pure id <*> sampleValueNonEmpty1) sampleValueNonEmpty1
    shouldBe
      (pure (.) <*> sampleFunctionNonEmpty1 <*> sampleFunctionNonEmpty2 <*> sampleValueNonEmpty1)
      (sampleFunctionNonEmpty1 <*> (sampleFunctionNonEmpty2 <*> sampleValueNonEmpty1))
    shouldBe
      (pure (.) <*> sampleFunctionNonEmpty1 <*> sampleFunctionNonEmpty2 <*> sampleValueNonEmpty2)
      (sampleFunctionNonEmpty1 <*> (sampleFunctionNonEmpty2 <*> sampleValueNonEmpty2))
    shouldBe ((pure sampleFunction :: NonEmpty (Integer -> Integer)) <*> pure sampleValue)
             (pure (sampleFunction sampleValue))
    shouldBe (sampleFunctionNonEmpty1 <*> pure sampleValue)
             (pure ($ sampleValue) <*> sampleFunctionNonEmpty1)

  let sampleMonadFunction1 = \x -> (x       :| [x + 1])
  let sampleMonadFunction2 = \x -> ((x + 1) :| [x + 2])
  it "instanceMonadNonEmpty" $ do
    shouldBe (return sampleValue >>= sampleMonadFunction1)
             (sampleMonadFunction1 sampleValue)
    shouldBe (sampleValueNonEmpty1 >>= return) sampleValueNonEmpty1
    shouldBe ((sampleValueNonEmpty1 >>= sampleMonadFunction1) >>= sampleMonadFunction2)
             (sampleValueNonEmpty1 >>= (\x -> sampleMonadFunction1 x >>= sampleMonadFunction2))

  it "instanceFoldableNonEmpty" $ do
    shouldBe (foldl (+) 0 sampleValueNonEmpty1) 6
    shouldBe (foldr (:) [] sampleValueNonEmpty2) [4, 5, 6]

  it "instanceTraversableNonEmpty" $ do
    shouldBe (traverse Identity sampleValueNonEmpty1) (Identity sampleValueNonEmpty1)
    shouldBe (traverse Just sampleValueNonEmpty1) (Just sampleValueNonEmpty1)


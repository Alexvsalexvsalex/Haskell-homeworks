module Block2Spec
  (
    spec
  ) where

import           Test.Hspec
import           Control.Exception (evaluate)
import qualified Data.List.NonEmpty as NE

import Block1
import Block2

spec :: Spec
spec = do
  let emptyTree = Leaf :: Tree Int
  let manyElemTree =
          Node
            (1 NE.:| [1])
            (Node (0 NE.:| []) (Node (-1 NE.:| []) Leaf Leaf) Leaf)
            (Node (5 NE.:| [5, 5]) Leaf Leaf)

  it "foldr" $ do
    shouldBe (foldr (:) [] emptyTree) []
    shouldBe (foldr (:) [] manyElemTree) [-1, 0, 1, 1, 5, 5, 5]
    shouldBe (foldr (+) 0 emptyTree) 0
    shouldBe (foldr (+) 0 manyElemTree) 16

  it "splitOn" $ do
    shouldBe (splitOn '/' (NE.fromList "path/to/file"))
      (NE.fromList ["path", "to", "file"])
    shouldBe (splitOn 'a' (NE.fromList "a")) (NE.fromList [[], []])

  it "joinWith" $ do
    shouldThrow (evaluate (joinWith 'a' (NE.fromList [[]])))
      (errorCall "result is empty list")
    shouldBe (joinWith '/' . splitOn '/' $ NE.fromList "path/to/file")
      (NE.fromList "path/to/file")

module Block1Spec
  (
    spec
  ) where

import           Test.Hspec
import qualified Data.List.NonEmpty as NE

import Block1

spec :: Spec
spec = do
  it "nextDay" $ do
    shouldBe (nextDay Monday)    Tuesday
    shouldBe (nextDay Wednesday) Thursday
    shouldBe (nextDay Thursday)  Friday
    shouldBe (nextDay Sunday)    Monday

  it "afterDays" $ do
    shouldBe (afterDays Monday)    Sunday
    shouldBe (afterDays Wednesday) Tuesday
    shouldBe (afterDays Saturday)  Friday
    shouldBe (afterDays Sunday)    Saturday

  it "isWeekend" $ do
    shouldBe (isWeekend Monday)   False
    shouldBe (isWeekend Thursday) False
    shouldBe (isWeekend Saturday) True
    shouldBe (isWeekend Sunday)   True

  it "daysToParty" $ do
    shouldBe (daysToParty Monday)    4
    shouldBe (daysToParty Wednesday) 2
    shouldBe (daysToParty Friday)    0
    shouldBe (daysToParty Saturday)  6

  let zero = Z
  let one = S zero
  let two = S one
  let three = S two
  let four = S three
  let five = S four
  it "natAdd" $ do
    shouldBe (two + one)  three
    shouldBe (zero + two) two

  it "natMul" $ do
    shouldBe (zero * one) zero
    shouldBe (two * two)  four

  it "natNum" $ do
    shouldBe (toInteger' zero)  0
    shouldBe (toInteger' three) 3
    shouldBe (fromInteger 0)    zero
    shouldBe (fromInteger 2)    two

  it "natEq" $ do
    shouldBe (zero == zero) True
    shouldBe (two == two)   True
    shouldBe (zero /= one)  True
    shouldBe (two == one)   False

  it "natOrd" $ do
    shouldBe (zero > one) False
    shouldBe (one >= one) True
    shouldBe (one < two)  True
    shouldBe (one >= two) False

  it "natEven" $ do
    shouldBe (isEven zero)  True
    shouldBe (isEven two)   True
    shouldBe (isEven one)   False
    shouldBe (isEven three) False

  it "natQuotRem" $ do
    shouldBe (quotRem' five two)  (2, 1)
    shouldBe (quotRem' three one) (3, 0)
    shouldBe (quotRem' zero two)  (0, 0)
    shouldBe (quotRem' one three) (0, 1)

  let emptyTree = Leaf
  let oneElemTree = Node (1 NE.:| []) Leaf Leaf
  let manyElemTree =
        Node
          (1 NE.:| [1])
          (Node (0 NE.:| []) (Node (-1 NE.:| []) Leaf Leaf) Leaf)
          (Node (5 NE.:| [5, 5]) Leaf Leaf)
  it "isEmpty" $ do
    shouldBe (isEmpty emptyTree)    True
    shouldBe (isEmpty oneElemTree)  False
    shouldBe (isEmpty manyElemTree) False

  it "size" $ do
    shouldBe (size emptyTree)    0
    shouldBe (size oneElemTree)  1
    shouldBe (size manyElemTree) 7

  it "find" $ do
    shouldBe (find oneElemTree 2)  Nothing
    shouldBe (find manyElemTree 5) (Just (Node (5 NE.:| [5, 5]) Leaf Leaf))

  it "insert" $ do
    shouldBe (insert oneElemTree 1) (Node (1 NE.:| [1]) Leaf Leaf)
    shouldBe (insert oneElemTree 2)
      (Node (1 NE.:| []) Leaf (Node (2 NE.:| []) Leaf Leaf))

  it "fromList" $ do
    shouldBe (fromList [1]) oneElemTree
    shouldBe (fromList [1, 0, 5, 1, 5, -1, 5]) manyElemTree

  it "erase" $ do
    shouldBe (erase oneElemTree 1) emptyTree
    shouldBe
      (erase manyElemTree 0)
      (Node
        (1 NE.:| [1])
        (Node (-1 NE.:| []) Leaf Leaf)
        (Node (5 NE.:| [5, 5]) Leaf Leaf)
      )
    shouldBe
      (eraseAll manyElemTree 5)
      (Node
        (1 NE.:| [1])
        (Node (0 NE.:| []) (Node (-1 NE.:| []) Leaf Leaf) Leaf)
        Leaf
      )

module Block3Spec
  (
    spec
  ) where

import Control.Applicative
import Test.Hspec

import Block3

spec :: Spec
spec = do
  let sampleParser = (Parser $ \s -> Just (s, [])) :: Parser Char [Char]
  let failParser = empty :: Parser Char [Char]
  it "parserInstances" $ do
    shouldBe (runParser sampleParser "123") (Just ("123", ""))
    shouldBe (runParser (fmap read sampleParser) "123") (Just (123, ""))
    shouldBe (runParser (pure id <*> sampleParser) "111") (Just ("111", ""))
    shouldBe (runParser (return "000") "2") (Just ("000", "2"))
    shouldBe (runParser (failParser <|> sampleParser) "987") (Just ("987", ""))
    shouldBe (runParser (failParser <|> failParser <|> failParser) "333") Nothing

  it "basicCombinators" $ do
    shouldBe (runParser ok "_") (Just ((), "_"))
    shouldBe (runParser eof "_") Nothing
    shouldBe (runParser eof "") (Just ((), ""))
    shouldBe (runParser (element '_') "_123") (Just ('_', "123"))
    shouldBe (runParser (stream "321") "321ax") (Just ("321", "ax"))
    shouldBe (runParser (stream "31") "321ax") Nothing

  it "parsingBracketsAndIntegers" $ do
    shouldBe (runParser bracketsParser "(())") (Just ((), ""))
    shouldBe (runParser bracketsParser "()()(())") (Just ((), ""))
    shouldBe (runParser bracketsParser ")(())") Nothing
    shouldBe (runParser bracketsParser "()()())") Nothing
    shouldBe (runParser numberParser "-123") (Just (-123, ""))
    shouldBe (runParser numberParser "+13") (Just (13, ""))
    shouldBe (runParser numberParser "+00013") (Just (13, ""))
    shouldBe (runParser numberParser "99") (Just (99, ""))
    shouldBe (runParser numberParser "hfj234") Nothing

  it "listlistParser" $ do
    shouldBe (runParser listlistParser "1, 23") (Just ([[23]], ""))
    shouldBe (runParser listlistParser "2,0 , 23") (Just ([[0, 23]], ""))
    shouldBe (runParser listlistParser "9999, 23") Nothing
    shouldBe (runParser listlistParser "4; 23;1   ,3?1") Nothing
    shouldBe (runParser listlistParser "0,0,0,0,0") (Just ([[],[],[],[],[]], ""))
    shouldBe (runParser listlistParser "1 ,8, 0, 4, 1,5 ,7, 54,1, -8")
             (Just ([[8], [], [1, 5, 7, 54], [-8]], ""))


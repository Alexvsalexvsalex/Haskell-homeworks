module MockTestsSpec
  (
    spec
  ) where

import Control.Monad.Except
  (
    ExceptT
  , lift
  , runExceptT
  , throwError)
import Control.Monad.State
  (
    State
  , get
  , put
  , runState)
import Test.Hspec
  (
    Spec
  , it
  , shouldBe)

import Lib
  (
    FSActions(..)
  , MyException(..)
  , MockFS
  , PathSeq
  , SimFS(..))

runTest :: ExceptT MyException (State (PathSeq, SimFS)) a
        -> (Either MyException a, (PathSeq, SimFS))
runTest test = runState (runExceptT test) ([], SimDir "" [])

spec :: Spec
spec = do
  it "simpleTest" $ do
    shouldBe
      (runTest $ do
        createFolder "folder"
        writeToFile "file" "text"
        cd "folder"
        writeToFile "file2" "text"
        )
      (Right (), (["folder"],
        SimDir "" [ SimFile "file" "text"
                  , SimDir "folder" [SimFile "file2" "text"]]))

    shouldBe
      (runTest $ do
        createFolder "folder"
        createFile "file"
        createFile "file2"
        remove "file"
        remove "folder"
        )
      (Right (), ([],
        SimDir "" [SimFile "file2" ""]))

    shouldBe
      (runTest $ do
        createFolder "folder"
        cd "folder"
        cd ".."
        cd ".."
        )
      (Left GoOutOfRoot, ([],
        SimDir "" [SimDir "folder" []]))

    shouldBe
      (runTest $ do
        createFolder "folder"
        cd "folder"
        writeToFile "file" "echo"
        cat "file"
        )
      (Right "echo", (["folder"],
        SimDir "" [SimDir "folder" [SimFile "file" "echo"]]))

    shouldBe
      (runTest $ do
        createFolder "folder"
        cd "folder"
        createFile "file"
        createFile "file2"
        cd ".."
        createFile "file3"
        ls "folder"
        )
      (Right ["file2", "file"], ([],
        SimDir "" [ SimFile "file3" ""
                  , SimDir "folder" [ SimFile "file2" ""
                                    , SimFile "file" ""]]))

    shouldBe
      (runTest $ do
        findFile' "file4"
        )
      (Right [], ([],
        SimDir "" []))

    shouldBe
      (runTest $ do
        writeToFile "file4" "123"
        findFile' "file4"
        )
      (Right ["file4"], ([],
        SimDir "" [SimFile "file4" "123"]))

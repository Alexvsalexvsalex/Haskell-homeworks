{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  (
    FSActions(..)
  , MyException(..)
  , MockFS
  , PathSeq
  , RealFS
  , SimFS(..)
  ) where

import Control.Monad.Except
  (
    ExceptT
  , lift
  , throwError)
import Control.Monad.State
  (
    State
  , get
  , put)
import Control.Monad.Trans.Reader
  (
    ReaderT
  , ask)
import Data.IORef
  (
    IORef
  , readIORef
  , writeIORef)
import System.Directory
  (
    createDirectory
  , doesDirectoryExist
  , findFiles
  , listDirectory
  , removePathForcibly)
import System.FilePath.Posix(
  (</>))

data MyException
  = GoOutOfRoot
  | PathNotExists
  | CannotCreateFile
  | CannotCreateFolder
  | CannotOpenFile
  | CannotRemove
  | CannotWriteFile deriving (Show, Eq)

class FSActions m where
  cd :: FilePath -> m ()
  dir :: m [FilePath]
  ls :: FilePath -> m [FilePath]
  createFolder :: FilePath -> m ()
  cat :: FilePath -> m String
  createFile :: FilePath -> m ()
  remove :: FilePath -> m ()
  writeToFile :: FilePath -> String -> m ()
  findFile' :: FilePath -> m [FilePath]

-- Real file system
type RealFS = ReaderT (IORef FilePath) IO

getCurDir :: RealFS String
getCurDir = do
  ioRef <- ask
  lift $ readIORef ioRef

instance FSActions RealFS where
  cd p = do
    ioRef <- ask
    curCd <- lift $ readIORef ioRef
    let nxtCd = curCd </> p
    valid <- lift $ doesDirectoryExist nxtCd
    if valid then
      lift $ writeIORef ioRef nxtCd
    else
      lift . putStrLn $ "Path " ++ nxtCd ++ " doesn't exist"
  dir = ls ""
  ls path = do
    curD <- getCurDir
    lift $ listDirectory (curD </> path)
  createFolder folderName = do
    curD <- getCurDir
    lift $ createDirectory (curD </> folderName)
  cat fileName = do
    curD <- getCurDir
    lift $ readFile (curD </> fileName)
  remove objectName = do
    curD <- getCurDir
    lift $ removePathForcibly (curD </> objectName)
  createFile fileName = writeToFile fileName ""
  writeToFile fileName text = do
    curD <- getCurDir
    lift $ writeFile (curD </> fileName) text
  findFile' fileName = do
    curD <- getCurDir
    lift $ findFiles [curD] fileName

-- Fake file system
type PathSeq = [String]
data SimFS = SimDir FilePath [SimFS] | SimFile FilePath String deriving (Show, Eq)

type MockFS = ExceptT MyException (State (PathSeq, SimFS))

getObjName :: SimFS -> FilePath
getObjName (SimDir nm _) = nm
getObjName (SimFile nm _) = nm

modSubFS :: PathSeq -> (SimFS -> (Maybe a, SimFS)) -> SimFS -> (Maybe a, SimFS)
modSubFS [] func fs = func fs
modSubFS _  _ sf@(SimFile _ _) = (Nothing, sf)
modSubFS (x:xs) func (SimDir nm subObj) =
  let (res, sub) = helper subObj in (res, SimDir nm sub)
  where
    helper [] = (Nothing, [])
    helper (a:as) =
      if getObjName a == x
      then let (m, sf) = modSubFS xs func a in (m, sf:as)
      else let (m, sfs) = helper as in (m, a:sfs)

lsImpl :: SimFS -> (Maybe [FilePath], SimFS)
lsImpl sd@(SimDir _ lst) = (Just $ map getObjName lst, sd)
lsImpl oth = (Nothing, oth)

procModSubFS :: (Maybe a, SimFS) -> MyException -> MockFS a
procModSubFS (Just x, fs) _ = do
  (oldP, _) <- lift $ get
  lift $ put (oldP, fs)
  return x
procModSubFS _ s = throwError s

createObjImpl :: Bool -> SimFS -> SimFS -> (Maybe (), SimFS)
createObjImpl replace nObj sd@(SimDir nm lst) =
  let fLst = filter (\t -> getObjName t /= getObjName nObj) lst in
  if (length fLst) < (length lst) && not replace
  then (Nothing, sd)
  else (Just (), (SimDir nm (nObj:fLst)))
createObjImpl _ _ sf = (Nothing, sf)

catImpl :: SimFS -> (Maybe String, SimFS)
catImpl sf@(SimFile _ txt) = (Just txt, sf)
catImpl oth = (Nothing, oth)

removeObjImpl :: FilePath -> SimFS -> (Maybe (), SimFS)
removeObjImpl path sd@(SimDir nm lst) =
  let fLst = filter (\t -> getObjName t /= path) lst in
  if length fLst == length lst then (Nothing, sd) else (Just (), (SimDir nm fLst))
removeObjImpl _ sf = (Nothing, sf)

findFileImpl :: FilePath -> SimFS -> (Maybe [FilePath], SimFS)
findFileImpl fileName sd@(SimDir _ lst) =
  if any (\t -> getObjName t == fileName) lst
  then (Just [fileName], sd)
  else (Just [], sd)
findFileImpl _ sf = (Nothing, sf)

instance FSActions MockFS where
  cd p = do
    (path, fs) <- lift $ get
    if p == ".."
    then if length path == 0
         then throwError GoOutOfRoot
         else lift $ put (init path, fs)
    else lift $ put (path ++ [p], fs)
  dir = do
    (path, fs) <- lift $ get
    procModSubFS
      (modSubFS path lsImpl fs)
      PathNotExists
  ls p = do
    (path, fs) <- lift $ get
    procModSubFS
      (modSubFS (path ++ [p]) lsImpl fs)
      PathNotExists
  createFolder folderName = do
    (path, fs) <- lift $ get
    procModSubFS
      (modSubFS path (createObjImpl False $ SimDir folderName []) fs)
      CannotCreateFolder
  cat fileName = do
    (path, fs) <- lift $ get
    procModSubFS
      (modSubFS (path ++ [fileName]) catImpl fs)
      CannotOpenFile
  remove obj = do
    (path, fs) <- lift $ get
    procModSubFS
      (modSubFS path (removeObjImpl obj) fs)
      CannotRemove
  createFile fileName = do
    (path, fs) <- lift $ get
    procModSubFS
      (modSubFS path (createObjImpl False $ SimFile fileName "") fs)
      CannotCreateFile
  writeToFile fileName text = do
    (path, fs) <- lift $ get
    procModSubFS
      (modSubFS path (createObjImpl True $ SimFile fileName text) fs)
      CannotWriteFile
  findFile' fileName = do
    (path, fs) <- lift $ get
    procModSubFS
      (modSubFS path (findFileImpl fileName) fs)
      PathNotExists

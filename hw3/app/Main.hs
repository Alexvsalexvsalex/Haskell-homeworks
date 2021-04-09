module Main
  (
    main
  ) where

import Control.Monad.Trans
  (
    lift
  , liftIO)
import Control.Monad.Trans.Reader
  (
    ask
  , runReaderT)
import Data.IORef
  (
    IORef
  , newIORef
  , readIORef)
import Options.Applicative
  (
    Parser
  , ParserInfo
  , ParserResult(Failure)
  , argument
  , command
  , defaultPrefs
  , execParserPure
  , getParseResult
  , info
  , metavar
  , progDesc
  , renderFailure
  , str
  , subparser)
import System.Environment
  (
    getArgs)
import System.IO
  (
    stdout
  , hFlush)
import System.IO.Error
  (
    catchIOError)

import Lib
  (
    FSActions(..)
  , RealFS)

data Command
  = Cd FilePath
  | Dir
  | Ls FilePath
  | CreateFolder FilePath
  | Cat FilePath
  | CreateFile FilePath
  | Remove FilePath
  | WriteToFile FilePath String
  | FindFile FilePath
  | Help
  | Exit
  | Incorrect String
  deriving (Eq)

specInfo :: Parser a -> String -> ParserInfo a
specInfo opts desc = info opts $ progDesc desc

parserCmd :: String -> ParserInfo Command
parserCmd cmd = case cmd of
  "cd" ->
    (Cd <$>
      argument str (metavar "path")) `specInfo` "перейти в директорию"
  "dir" ->
    (pure Dir) `specInfo` "показать содержимое текущей директории"
  "ls" ->
    (Ls <$>
      argument str (metavar "path")) `specInfo` "показать содержимое выбранной директории"
  "create-folder" ->
    (CreateFolder <$>
      argument str (metavar "folder_name")) `specInfo` "создать директорию в текущей"
  "cat" ->
    (Cat <$>
      argument str (metavar "file_name")) `specInfo` "показать содержимое файла"
  "create-file" ->
    (CreateFile <$>
      argument str (metavar "file_name")) `specInfo` "создать пустой файл в текущей директории"
  "remove" ->
    (Remove <$>
      argument str (metavar "path")) `specInfo` "удалить выборанную директорию или файл"
  "write-file" ->
    (WriteToFile <$>
      argument str (metavar "file_name") <*>
      argument str (metavar "text")) `specInfo` "записать текст в файл"
  "find-file" ->
    (FindFile <$>
      argument str (metavar "file_name")) `specInfo` "поиск файла в текущией директории"
  "help" ->
    (pure Help) `specInfo` "показать руководство по использованию"
  "exit" ->
    (pure Exit) `specInfo` "завершение работы программы"
  _ ->
    error "unsupported command"

parserCommand :: Parser Command
parserCommand = subparser $
    foldMap (\s -> command s (parserCmd s)) ["cd", "dir", "ls", "create-folder",
      "cat", "create-file", "remove", "write-file", "find-file", "help", "exit"]

procResult :: ParserResult Command -> IO Command
procResult (Failure failure) =
  return . Incorrect . fst $ renderFailure failure ""
procResult suc = case getParseResult suc of
  Just res -> return res
  Nothing  -> return $ Incorrect ""

fullParser :: ParserInfo Command
fullParser = parserCommand `specInfo` "FileManager"

printPrefix :: RealFS ()
printPrefix = do
  ioRef <- ask
  res <- lift $ readIORef ioRef
  lift $ putStr (res ++ " > ")
  lift $ hFlush stdout

cmdFM :: RealFS ()
cmdFM = do
  printPrefix
  rawLn <- lift $ getLine
  com <- lift . procResult $
    execParserPure defaultPrefs fullParser (words rawLn)
  case com of
    Cd path -> cd path
    Dir -> do
      arr <- dir
      liftIO $ mapM_ putStrLn arr
    Ls path -> do
      arr <- ls path
      liftIO $ mapM_ putStrLn arr
    CreateFolder folderName -> createFolder folderName
    Cat fileName -> do
      arg <- cat fileName
      liftIO $ putStrLn arg
    CreateFile fileName -> createFile fileName
    Remove objectName -> remove objectName
    WriteToFile fileName text -> writeToFile fileName text
    FindFile fileName -> do
      arr <- findFile' fileName
      liftIO $ mapM_ putStrLn arr
    Help -> liftIO $ do
      putStrLn "cd <folder> -- перейти в директорию"
      putStrLn "dir -- показать содержимое текущей директории"
      putStrLn "ls <folder> -- показать содержимое выбранной директории"
      putStrLn "create-folder 'folder-name' -- создать директорию в текущей"
      putStrLn "cat <file> -- показать содержимое файла"
      putStrLn "create-file 'file-name' -- создать пустой файл в текущей директории"
      putStrLn "remove <folder | file> -- удалить выборанную директорию или файл"
      putStrLn "write-file <file> 'text' -- записать текст в файл"
      putStrLn "find-file 'file-name' --  поиск файла в текущией директории"
      putStrLn "help --  показать руководство по использованию"
      putStrLn "exit -- завершение работы программы"
    Incorrect msg -> liftIO $ putStrLn msg
    Exit -> return ()
  if com == Exit
  then return ()
  else cmdFM

starter :: IORef FilePath -> IO ()
starter raw = do
  (runReaderT cmdFM raw) `catchIOError`
    (\e -> do
      putStrLn $ "Error handled\n" ++ show e
      starter raw)

main :: IO ()
main = do
  runDir <- getArgs
  raw <- newIORef . head $ runDir
  starter raw

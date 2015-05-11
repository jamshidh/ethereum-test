module TestFiles (
  TestTree(..),
  load
  ) where

-- External function modules
import Control.Monad (mapM)
import Data.Functor ((<$>))
import Data.List (isSuffixOf)
import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist)
import System.FilePath (takeExtension, takeFileName, (</>))

-- Internal modules
import TestDescriptions (Tests)
import qualified JSON

loadBlacklist dir = do
  let absFile = dir </> ".blacklist"
  isBlacklist <- doesFileExist absFile
  if isBlacklist
    then map (dir </>) . lines <$> readFile absFile
    else return []

data FileType = Directory | File | NotThere

fileType :: Bool -> Bool -> FileType
fileType True False = Directory
fileType False True = File
fileType _ _ = NotThere

type Filename = String

data TestTree =
  TestDir  { fileName::Filename, dirEntries::[TestTree] } |
  TestFile { fileName::Filename, fileTests::Tests } |
  NoTests { fileName::Filename } |
  Ignore { fileName::Filename }
  deriving (Show)

load::String -> IO TestTree
load base = do  
  isDir <- doesDirectoryExist base
  isFile <- doesFileExist base
  loadTest [] base $ fileType isDir isFile
  where    
    testsInDir blacklist0 dir =
      if dir `elem` blacklist0
      then return $ Ignore dir
      else do
        blacklist <- (++ blacklist0) <$> loadBlacklist dir
        rawContents <- getDirectoryContents dir
        let rawContents' = map (dir </>) rawContents
        contents <- mapM (loadIfTest blacklist) rawContents'
        return $
          if null contents
          then NoTests dir
          else TestDir dir contents
               
    testFile blacklist file =
      if file `elem` blacklist
      then return $ Ignore file
      else do
        fileTests' <- JSON.load file
        return $ TestFile file fileTests'

    loadIfTest blacklist name = do
      isDir <- doesDirectoryExist name
      isFile <- doesFileExist name
      let fname = takeFileName name
          hasTests = "Tests" `isSuffixOf` fname
          hasJson  = ".json" == takeExtension fname
          isVisible = null fname || head (takeFileName name) /= '.'
      if not isVisible || (isDir && not hasTests) || (isFile && not hasJson)
        then return $ Ignore name
        else loadTest blacklist name $ fileType isDir isFile
      
    loadTest blacklist name Directory = testsInDir blacklist name
    loadTest blacklist name File = testFile blacklist name
    loadTest _ name NotThere = return $ NoTests name
      

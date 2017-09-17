{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  DirMetz.FileSys.Base
-- Copyright   :  (c) Stephen Tetley 2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Generic traversals for External syntax.
--
--------------------------------------------------------------------------------


module DirMetz.FileSys.Base where


import Data.Time                        -- package: time

import Control.Monad
import Data.List ( foldl', sort )
import System.Directory
import System.FilePath


hoursMinutesSecondsToDiffTime :: Int -> Int -> Integer -> DiffTime
hoursMinutesSecondsToDiffTime h m s = secondsToDiffTime $ s + 60 * (fromIntegral m) + 3600 * (fromIntegral h)


type Name = String
type Size = Integer

-- Trees have a pendant which stores different information
data FileStore = FileStore FilePath [FileObj]
  deriving (Eq,Ord,Show)

data FileObj = Folder Name Properties [FileObj]
             | File   Name Properties Size
  deriving (Eq,Ord,Show)

-- Note - adding properties is onerous for dir listings. 
--
-- We can always get modification time / access time from the 
-- FileSystem, but if we are making a FileObj from the output
-- of dir times may not be available.
--
-- It seems prudent to maybe-fy them, metrics are going to 
-- have to deal with bad data at sme point anyway.
--
data Properties = Properties
    { access_time       :: Maybe UTCTime
    , modification_time :: Maybe UTCTime
    }
  deriving (Eq,Ord,Show)

-- Other possible file stats are premissions...


--------------------------------------------------------------------------------
-- Populate 

populateFS :: FilePath -> IO FileStore
populateFS root = FileStore root <$> children root
  where
    listDirectoryLong :: FilePath -> IO [FilePath]
    listDirectoryLong path = map (path </>) <$> listDirectory path

    children path = do { kids  <- filterM doesDirectoryExist =<< listDirectoryLong path 
                       ; kids' <- mapM folder1 kids
                       ; files <- files1 path
                       ; return $ kids' ++ files }

    folder1 path  = do { props <- populateProperties path
                       ; kids  <- children path
                       ; return $ Folder (takeFileName path) props kids }
                       
    files1 path = do { xs <- filterM doesFileExist =<< listDirectoryLong path 
                     ; forM xs (\x -> do { props <- populateProperties x
                                         ; sz <- getFileSize x
                                         ; return $ File (takeFileName x) props sz }) 
                     }


populate :: FilePath -> IO FileObj
populate = foldersR
  where
    listDirectoryLong :: FilePath -> IO [FilePath]
    listDirectoryLong path = map (path </>) <$> listDirectory path

    foldersR path = do { props <- populateProperties path
                       ; kids  <- filterM doesDirectoryExist =<< listDirectoryLong path 
                       ; kids' <- mapM foldersR kids
                       ; files <- files1 path
                       ; return $ Folder (takeFileName path) props (kids' ++ files) }
                       
    files1 path = do { xs <- filterM doesFileExist =<< listDirectoryLong path 
                     ; forM xs (\x -> do { props <- populateProperties x
                                         ; sz <- getFileSize x
                                         ; return $ File (takeFileName x) props sz }) 
                     }


populateProperties :: FilePath -> IO Properties
populateProperties path = do 
    a <- getAccessTime path
    m <- getModificationTime path
    return $ Properties { access_time = Just a
                        , modification_time = Just m
                        }

--------------------------------------------------------------------------------
-- Display

-- The simplest useful rendering is a topdown list of paths of 
-- files and directories, one item per line, printed alphabetically.
--

-- Note we need to sort, currently the sort order favours directories and
-- is case sensitive

display :: FileStore -> String
display (FileStore path kids) = 
    ($ "") $ foldl' (\ac fo -> ac . display1 fo) (showString path) (sort kids)


display1 :: FileObj -> ShowS
display1 = step id ""
  where
    step ac path (File s _ _)    = ac `appendLine` (catPath path s)

    step ac path (Folder s _ xs) = let path1 = catPath path s
                                       ac1   = ac `appendLine` path1
                                   in foldl' (\acc fo -> step acc path1 fo) ac1 (sort xs)


catPath :: String -> String -> String
catPath s1 s2 | null s1 = s2
              | otherwise = s1 ++  ('\\':s2)


appendLine :: ShowS -> String -> ShowS
appendLine f s = let s1 = ('\n':s) in f . showString s1
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

import qualified Data.CaseInsensitive   as CI            -- package: case-insensitive

import Data.Time                        -- package: time

import Control.Monad
import Data.List
import System.Directory
import System.FilePath


hoursMinutesSecondsToDiffTime :: Int -> Int -> Integer -> DiffTime
hoursMinutesSecondsToDiffTime h m s = secondsToDiffTime $ s + 60 * (fromIntegral m) + 3600 * (fromIntegral h)


type Name = String
type Size = Integer

-- Trees have a pendant which stores different information
data FileStore = FileStore 
    { absolute_path     :: FilePath 
    , contents          :: [FileObj]
    }
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


nameOf :: FileObj -> String
nameOf (Folder s _ _)   = s
nameOf (File s _ _)     = s

neutralOrd :: FileObj -> FileObj -> Ordering
neutralOrd o1 o2 = CI.mk (nameOf o1) `compare` CI.mk (nameOf o2)



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

-- Note we need to sort, the sort order is neutral 
-- (does not favour files or directories, is case insenstive)

display :: FileStore -> String
display (FileStore path kids) = 
    ($ "") $ foldl' (\ac fo -> ac . display1 fo) 
                    (showString path) 
                    (sortBy neutralOrd kids)


display1 :: FileObj -> ShowS
display1 = step id ""
  where
    step ac path (File s _ _)    = ac `appendLine` (catPath path s)

    step ac path (Folder s _ xs) = 
         let path1 = catPath path s
             ac1   = ac `appendLine` path1
         in foldl' (\acc fo -> step acc path1 fo) ac1 (sortBy neutralOrd xs)


catPath :: String -> String -> String
catPath s1 s2 | null s1 = s2
              | otherwise = s1 ++  ('\\':s2)


appendLine :: ShowS -> String -> ShowS
appendLine f s = let s1 = ('\n':s) in f . showString s1

--------------------------------------------------------------------------------
-- Operations c.f. System.Directory


-- | If not a qualified path search from root of filestore

isPathPrefixOf :: FilePath -> FilePath -> Bool
isPathPrefixOf pre body = go (splitPath pre) (splitPath body)
  where
    go []     _                     = True
    go _      []                    = False     
    go (x:xs) (y:ys) 
       | normalise x == normalise y = go xs ys
       | otherwise                  = False

dropPathPrefix :: FilePath -> FilePath -> FilePath
dropPathPrefix pre body = go (splitPath pre) (splitPath body)
  where
    go []     rest                  = normalise $ joinPath rest
    go _      []                    = normalise ""
    go (x:xs) rest@(y:ys) 
       | normalise x == normalise y = go xs ys
       | otherwise                  = normalise $ joinPath rest


doesPathExist1 :: FilePath -> FileStore -> Bool
doesPathExist1 path store = go (splitPath path) (contents store)
  where
    getNames        = map (normalise . nameOf)
    go []     _     = False
    go [x]    fs    = normalise x `elem` getNames fs
    go (x:xs) fs    = 
       let x1 = normalise x in case find (\a -> x1 == nameOf a) fs of
           Just (Folder _ _ kids) -> go xs kids
           _  -> False

doesPathExist :: FilePath -> FileStore -> Bool
doesPathExist path store
    | not (hasDrive path)                       = doesPathExist1 path store
    | isPathPrefixOf (absolute_path store) path = 
        let suffix = dropPathPrefix (absolute_path store) path in doesPathExist1 suffix store
        
    | otherwise                                 = False



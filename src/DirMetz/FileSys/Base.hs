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
import Data.Maybe
import qualified System.Directory as BASE
import System.FilePath


hoursMinutesSecondsToDiffTime :: Int -> Int -> Integer -> DiffTime
hoursMinutesSecondsToDiffTime h m s = secondsToDiffTime $ s + 60 * (fromIntegral m) + 3600 * (fromIntegral h)


type Name = String
type Size = Integer

-- Trees have a pendant which stores different information
data FileStore = FileStore 
    { absolute_path     :: FilePath 
    , contents          :: [Content]
    }
  deriving (Eq,Ord,Show)

data Content = FsFolder Name Properties [Content]
             | FsFile   Name Properties Size
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


nameOf :: Content -> String
nameOf (FsFolder s _ _) = s
nameOf (FsFile s _ _)   = s

neutralOrd :: Content -> Content -> Ordering
neutralOrd o1 o2 = CI.mk (nameOf o1) `compare` CI.mk (nameOf o2)



--------------------------------------------------------------------------------
-- Populate 

readFromDisk :: FilePath -> IO FileStore
readFromDisk root = FileStore root <$> children root
  where
    listDirectoryLong :: FilePath -> IO [FilePath]
    listDirectoryLong path = map (path </>) <$> BASE.listDirectory path

    children path = 
        do { kids  <- filterM BASE.doesDirectoryExist =<< listDirectoryLong path 
           ; kids' <- mapM folder1 kids
           ; files <- files1 path
           ; return $ kids' ++ files }

    folder1 path  = 
        do { props <- populateProperties path
           ; kids  <- children path
           ; return $ FsFolder (takeFileName path) props kids }
                       
    files1 path = 
        do { xs <- filterM BASE.doesFileExist =<< listDirectoryLong path 
           ; forM xs (\x -> do { props <- populateProperties x
                               ; sz <- BASE.getFileSize x
                               ; return $ FsFile (takeFileName x) props sz }) 
           }

{-
populateC :: FilePath -> IO Content
populateC = foldersR
  where
    listDirectoryLong :: FilePath -> IO [FilePath]
    listDirectoryLong path = map (path </>) <$> BASE.listDirectory path

    foldersR path = 
        do { props <- populateProperties path
           ; kids  <- filterM BASE.doesDirectoryExist =<< listDirectoryLong path 
           ; kids' <- mapM foldersR kids
           ; files <- files1 path
           ; return $ FsFolder (takeFileName path) props (kids' ++ files) }
                       
    files1 path = 
        do { xs <- filterM BASE.doesFileExist =<< listDirectoryLong path 
           ; forM xs (\x -> do { props <- populateProperties x
                               ; sz <- BASE.getFileSize x
                               ; return $ FsFile (takeFileName x) props sz }) 
           }

-}


populateProperties :: FilePath -> IO Properties
populateProperties path = do 
    a <- BASE.getAccessTime path
    m <- BASE.getModificationTime path
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


display1 :: Content -> ShowS
display1 = step id ""
  where
    step ac path (FsFile s _ _)    = ac `appendLine` (catPath path s)

    step ac path (FsFolder s _ xs) = 
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


findFileObj1 :: FilePath -> FileStore -> Maybe Content
findFileObj1 path store = go (splitPath path) (contents store)
  where
    go []     _     = Nothing
    go [x]    objs  = let x1 = normalise x in find (\a -> x1 == nameOf a) objs
    go (x:xs) objs  = 
       let x1 = normalise x in case find (\a -> x1 == nameOf a) objs of
           Just (FsFolder _ _ kids) -> go xs kids
           _  -> Nothing


findFileObj :: FilePath -> FileStore -> Maybe Content
findFileObj path store              
    | not (hasDrive path)                       = findFileObj1 path store
    | isPathPrefixOf (absolute_path store) path = 
        let suffix = dropPathPrefix (absolute_path store) path 
        in findFileObj1 suffix store
  
    | otherwise                                 = Nothing


doesPathExist :: FilePath -> FileStore -> Bool
doesPathExist path store = isJust $ findFileObj path store             

doesFileExist :: FilePath -> FileStore -> Bool
doesFileExist path store = case findFileObj path store of
    Just (FsFile {}) -> True
    _ -> False

doesFolderExist :: FilePath -> FileStore -> Bool
doesFolderExist path store = case findFileObj path store of
    Just (FsFolder {}) -> True
    _ -> False



listDirectory :: FilePath -> FileStore -> [FilePath]
listDirectory path store = case findFileObj path store of
    Just (FsFolder _ _ objs) -> map (\a -> path </> nameOf a) objs
    _ -> []


findFile :: [FilePath] -> String -> FileStore -> Maybe FilePath
findFile dirs name store = outer dirs
  where
    outer []        = Nothing
    outer (d:ds)    = case findFileObj d store of
        Just (FsFolder _ _ objs) -> case inner objs of
            Nothing -> outer ds
            ans -> fmap (d </>) ans
        _ -> outer ds

    inner ((FsFile n _ _):_) | n == name  = Just n
    inner (_:xs)                          = inner xs
    inner []                              = Nothing


findFiles :: [FilePath] -> String -> FileStore -> [FilePath]
findFiles dirs name store = concatMap outer dirs
  where
    outer dir = case findFileObj dir store of
        Just (FsFolder _ _ objs) -> case inner objs of
            Nothing -> []
            Just file -> [dir </> file]
        _ -> []

    inner ((FsFile n _ _):_) | n == name  = Just n
    inner (_:xs)                          = inner xs
    inner []                              = Nothing



getFileSize :: FilePath -> FileStore -> Maybe Integer
getFileSize path store = case findFileObj path store of
    Just (FsFile _ _ sz) -> Just sz
    _ -> Nothing


getDirectoryContents :: FilePath -> FileStore -> [FilePath]
getDirectoryContents = listDirectory

-- Note - we could simulate IO signatures by having FileStore in 
-- a reader monad, then we could have:
--
-- > getDirectoryContents :: FilePath -> FS [FilePath]


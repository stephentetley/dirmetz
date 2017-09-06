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
import System.Directory
import System.FilePath



type Name = String
type Size = Integer

data FileObj = File   Name Properties Size
             | Folder Name Properties [FileObj]
  deriving (Show)

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
  deriving (Show)

-- Other possible file stats are premissions...


--------------------------------------------------------------------------------
-- Populate 

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


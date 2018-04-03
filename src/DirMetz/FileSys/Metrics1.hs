{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  DirMetz.FileSys.Metrics1
-- Copyright   :  (c) Stephen Tetley 2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Initial metrics.
--
--------------------------------------------------------------------------------


module DirMetz.FileSys.Metrics1 where

import DirMetz.FileSys.Base
import DirMetz.FileSys.Kure

import MetricsLib.Base


import Language.KURE                    -- package: kure
import Control.Newtype                  -- package: newtype
import Data.Time                        -- package: time



-- import qualified Data.Map as Map
import Data.Semigroup hiding ( (<>) )





-- cf SDF Metz 
-- a calc__ function to run the traversal and return 
calcLargestFile :: FileStore -> Result Integer
calcLargestFile = runKureResultM . applyT largestFileU zeroContext . inject




-- max monoid
largestFileU :: TransformE U Integer
largestFileU = 
    positiveT "empty Filesys" $ fmap (fromIntegral . getMax) $ crushtdT $ promoteT largestFileC
  where
    largestFileC :: TransformE Content Maxi
    largestFileC = 
        do FsFile _ _ sz <- idR
           return $ maxi sz


-- Latest file
calcLatestFile :: FileStore -> Result UTCTime
calcLatestFile = runKureResultM . applyT latestFileU zeroContext . inject
  


-- Latest monoid -- Maybe inside Latest becomes a failing strategy...
latestFileU :: TransformE U UTCTime
latestFileU = swapMaybeT "empty Filesys" $ fmap unpack $ crushtdT $ promoteT latestFileC
  where
    latestFileC :: TransformE Content Latest
    latestFileC = 
        do FsFile _ props _ <- idR
           return $ Latest $ modification_time props



calcEarliestFile :: FileStore -> Result UTCTime
calcEarliestFile = runKureResultM . applyT earliestFileU zeroContext . inject

earliestFileU :: TransformE U UTCTime
earliestFileU = 
    swapMaybeT "empty Filesys" $ fmap getEarliest $ crushtdT $ promoteT earliestFileC
  where
    earliestFileC :: TransformE Content Earliest
    earliestFileC = 
        do FsFile _ props _ <- idR
           return $ Earliest $ modification_time props





-- Counting 
-- Counting doesn't seem to need labelling unless we label the 
-- metric itself, although that seems unnecessary at the element 
-- level as the metric name never changes.



calcCountFiles :: FileStore -> Result Integer
calcCountFiles = runKureResultM . applyT countFilesU zeroContext . inject
  where
    countFilesU :: TransformE U Integer
    countFilesU = fmap getSum $ crushtdT $ promoteT countFilesC


-- sum monoid
countFilesC :: TransformE Content (Sum Integer)
countFilesC = 
    do FsFile {} <- idR
       return 1


-- Note - we shouldn't count the root folder (to check...)
--
calcCountFolders :: FileStore -> Result Integer
calcCountFolders = runKureResultM . applyT countFoldersU zeroContext . inject
  where
    countFoldersU :: TransformE U Integer
    countFoldersU = fmap getSum $ crushtdT $ promoteT countFoldersC



countFoldersC :: TransformE Content (Sum Integer)
countFoldersC = 
    do FsFolder {} <- idR
       return 1


-- deepest 
-- deepest is not a straight-forward topdown traversal


calcMaxDepth :: FileStore -> Result Integer
calcMaxDepth = runKureResultM . applyT maxDepth1 zeroContext . inject

-- allT needs to (implicitly) use the Max monoid, which needs
-- Bounded on its number type.
--
maxDepth1 :: TransformE U Integer
maxDepth1 = fmap (fromIntegral . getMax) $ allT $ promoteT (folderDepth <+ fileDepth)
  where
    folderDepth :: TransformE Content Maxi
    folderDepth = 
        do FsFolder {} <- idR
           mx <- allT $ folderDepth <+ fileDepth
           return $ 1 + mx

    fileDepth :: TransformE Content Maxi
    fileDepth = 
        do FsFile {} <- idR 
           return $ maxi (1::Integer)



{-

-- Histogram
-- Design issue - histograms are more "queries" than "metrics".
--
type Histo = Map.Map String Integer

subsystems :: Content -> Either String Histo
subsystems = runKureM Right Left . applyT subsystems1 zeroContext

subsystems1 :: TransformE Content Histo
subsystems1 = fmap Map.fromList $ allT $ tryM [] go_folder
  where
    go_folder = do FsFolder s _ _ <- idR
                   i <- countFilesC
                   j <- countFoldersC
                   return [(s,i+j)]

-}
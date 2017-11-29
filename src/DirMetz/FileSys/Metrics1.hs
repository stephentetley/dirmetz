{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE UndecidableInstances       #-}
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

import Data.Time                        -- package: time



import qualified Data.Map as Map
import Data.Semigroup hiding ( (<>) )




-- 
fileSize1 :: TransformE FileObj (SizeMetric Integer)
fileSize1 = swapMaybeT "empty Filesys" $ fmap ((fmap measure1ToMetric) . getSizeMeasure) $ crushtdT $
    do File _ _ sz <- idR
       return $ sizeMeasure $ fromIntegral sz



-- cf SDF Metz 
-- a calc__ function to run the traversal and return 
calcLargestFile :: FileObj -> Result Integer
calcLargestFile = runKureResultM . applyT largestFile1 zeroContext




-- max monoid
largestFile1 :: TransformE FileObj Integer
largestFile1 = positiveT "empty Filesys" $ fmap (fromIntegral . getMax) $ crushtdT $ 
    do File _ _ sz <- idR
       return $ maxi sz


-- Latest file


calcLatestFile :: FileObj -> Result UTCTime
calcLatestFile = runKureResultM . applyT latestFile1 zeroContext
  


-- Latest monoid -- Maybe inside Latest becomes a failing strategy...
latestFile1 :: TransformE FileObj UTCTime
latestFile1 = swapMaybeT "empty Filesys" $ fmap getLatest $ crushtdT $ 
    do File _ props _ <- idR
       return $ Latest $ modification_time props



calcEarliestFile :: FileObj -> Result UTCTime
calcEarliestFile = runKureResultM . applyT earliestFile1 zeroContext

earliestFile1 :: TransformE FileObj UTCTime
earliestFile1 = swapMaybeT "empty Filesys" $ fmap getEarliest $ crushtdT $ 
    do File _ props _ <- idR
       return $ Earliest $ modification_time props





-- Counting 
-- Counting doesn't seem to need labelling unless we label the 
-- metric itself, although that seems unnecessary at the element 
-- level as the metric name never changes.



calcCountFiles :: FileObj -> Result Integer
calcCountFiles = runKureResultM . applyT countFiles1 zeroContext


-- sum monoid
countFiles1 :: TransformE FileObj Integer
countFiles1 = fmap getSum $ crushtdT $ 
    do File {} <- idR
       return 1



calcCountFolders :: FileObj -> Result Integer
calcCountFolders = runKureResultM . applyT countFolders1 zeroContext


-- Note - don't count the root folder (so don't use crushtdT)
--
-- Design note - this is implemented with a recursion that we 
-- could just as easily do without using strategies, can we do it better?
--
countFolders1 :: TransformE FileObj Integer
countFolders1 = fmap getSum go_kids
  where
    go_kids   = allT $ tryM 0 go_folder
    go_folder = do Folder {} <- idR
                   i <- go_kids
                   return $ 1 `mappend` i


-- deepest 
-- deepest is not a straight-forward topdown traversal


calcMaxDepth :: FileObj -> Result Integer
calcMaxDepth = runKureResultM . applyT maxDepth1 zeroContext

maxDepth1 :: TransformE FileObj Integer
maxDepth1 = fmap (fromIntegral . getMax) $ allT $ folder_depth <+ file_depth 
  where
    folder_depth = do Folder {} <- idR
                      mx <- allT $ folder_depth <+ file_depth
                      return $ 1 + mx
    file_depth = do File {} <- idR 
                    return $ maxi (1::Integer)




-- Histogram
-- Design issue - histograms are more "queries" than "metrics".
--
type Histo = Map.Map String Integer

subsystems :: FileObj -> Either String Histo
subsystems = runKureM Right Left . applyT subsystems1 zeroContext

subsystems1 :: TransformE FileObj Histo
subsystems1 = fmap Map.fromList $ allT $ tryM [] go_folder
  where
    go_folder = do Folder s _ _ <- idR
                   i <- countFiles1
                   j <- countFolders1
                   return [(s,i+j)]

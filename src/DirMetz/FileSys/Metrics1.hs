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



import qualified Data.Map as Map
import Data.Semigroup hiding ( (<>) )




-- 
fileSize1 :: TransformE Content (SizeMetric Integer)
fileSize1 = swapMaybeT "empty Filesys" $ fmap extractSizeMetricMb $ crushtdT $
    do FsFile _ _ sz <- idR
       return $ sizeMeasure $ fromIntegral sz



-- cf SDF Metz 
-- a calc__ function to run the traversal and return 
calcLargestFile :: FileStore -> Result Integer
calcLargestFile = runKureResultM . applyT largestFile1 zeroContext . inject




-- max monoid
largestFile1 :: TransformE U Integer
largestFile1 = positiveT "empty Filesys" $ fmap (fromIntegral . getMax) $ crushtdT $ largestFileU


largestFileU :: TransformE U Maxi
largestFileU = promoteT largestFileT

largestFileT :: TransformE Content Maxi
largestFileT = 
    do FsFile _ _ sz <- idR
       return $ maxi sz

-- Latest file


calcLatestFile :: Content -> Result UTCTime
calcLatestFile = runKureResultM . applyT latestFile1 zeroContext
  


-- Latest monoid -- Maybe inside Latest becomes a failing strategy...
latestFile1 :: TransformE Content UTCTime
latestFile1 = swapMaybeT "empty Filesys" $ fmap unpack $ crushtdT $ 
    do FsFile _ props _ <- idR
       return $ Latest $ modification_time props



calcEarliestFile :: Content -> Result UTCTime
calcEarliestFile = runKureResultM . applyT earliestFile1 zeroContext

earliestFile1 :: TransformE Content UTCTime
earliestFile1 = swapMaybeT "empty Filesys" $ fmap getEarliest $ crushtdT $ 
    do FsFile _ props _ <- idR
       return $ Earliest $ modification_time props





-- Counting 
-- Counting doesn't seem to need labelling unless we label the 
-- metric itself, although that seems unnecessary at the element 
-- level as the metric name never changes.



calcCountFiles :: Content -> Result Integer
calcCountFiles = runKureResultM . applyT countFiles1 zeroContext


-- sum monoid
countFiles1 :: TransformE Content Integer
countFiles1 = fmap getSum $ crushtdT $ 
    do FsFile {} <- idR
       return 1



calcCountFolders :: Content -> Result Integer
calcCountFolders = runKureResultM . applyT countFolders1 zeroContext


-- Note - don't count the root folder (so don't use crushtdT)
--
-- Design note - this is implemented with a recursion that we 
-- could just as easily do without using strategies, can we do it better?
--
countFolders1 :: TransformE Content Integer
countFolders1 = fmap getSum go_kids
  where
    go_kids   = allT $ tryM 0 go_folder
    go_folder = do FsFolder {} <- idR
                   i <- go_kids
                   return $ 1 `mappend` i


-- deepest 
-- deepest is not a straight-forward topdown traversal


calcMaxDepth :: Content -> Result Integer
calcMaxDepth = runKureResultM . applyT maxDepth1 zeroContext

-- allT needs to (implicitly) use the Max monoid, which needs
-- Bounded on its number type.
--
maxDepth1 :: TransformE Content Integer
maxDepth1 = fmap (fromIntegral . getMax) $ allT $ folder_depth <+ file_depth 
  where
    folder_depth = do FsFolder {} <- idR
                      mx <- allT $ folder_depth <+ file_depth
                      return $ 1 + mx

    file_depth = do FsFile {} <- idR 
                    return $ maxi (1::Integer)




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
                   i <- countFiles1
                   j <- countFolders1
                   return [(s,i+j)]

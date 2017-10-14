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
import DirMetz.Metrics


import Language.KURE                    -- package: kure


import Data.Int
import qualified Data.Map as Map
import Data.Semigroup hiding ( (<>) )



type Maxi = Max Int64

maxi :: Integral a => a -> Maxi
maxi = Max . fromIntegral


-- TODO - how (where?) do we accommodate metrics failing?
--
unsafeExtract :: a -> Either b a -> a
unsafeExtract a (Left _)  = a
unsafeExtract _ (Right a) = a

-- cf SDF Metz 
-- a calc__ function to run the traversal and return 
calcLargestFile :: FileObj -> Integer
calcLargestFile = 
    unsafeExtract 0 . runKureM Right Left . applyT largestFile1 zeroContext


largestFile :: FileObj -> Either String Integer
largestFile = runKureM Right Left . applyT largestFile1 zeroContext


-- max monoid
largestFile1 :: TransformE FileObj Integer
largestFile1 = fmap (fromIntegral . getMax) $ crushtdT $ 
    do File _ _ sz <- idR
       return $ maxi sz


-- Note - labelling no longer seems germane to the idea of metrics...

-- Labelling means we can identify the largest node alongside its size.

largestFileNamed :: FileObj -> Either String (String,Integer)
largestFileNamed = runKureM Right Left . applyT largestFileNamed1 zeroContext


-- max monoid
largestFileNamed1 :: TransformE FileObj (String,Integer)
largestFileNamed1 = fmap get $ crushtdT $ 
    do File s _ sz <- idR
       return $ LargestInteger $ Labelled s sz
  where
    get (LargestInteger (Labelled s i)) = (s,i)


-- Counting 
-- Counting doesn't seem to need labelling unless we label the 
-- metric itself, although that seems unnecessary at the element 
-- level as the metric name never changes.



countFiles :: FileObj -> Either String Integer
countFiles = runKureM Right Left . applyT countFiles1 zeroContext


-- sum monoid
countFiles1 :: TransformE FileObj Integer
countFiles1 = fmap getSum $ crushtdT $ 
    do File {} <- idR
       return 1

countFolders :: FileObj -> Either String Integer
countFolders = runKureM Right Left . applyT countFolders1 zeroContext


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


maxDepth :: FileObj -> Either String Integer
maxDepth = runKureM Right Left . applyT maxDepth1 zeroContext

maxDepth1 :: TransformE FileObj Integer
maxDepth1 = fmap (fromIntegral . getMax) $ allT $ folder_depth <+ file_depth 
  where
    folder_depth = do Folder {} <- idR
                      mx <- allT $ folder_depth <+ file_depth
                      return $ 1 + mx
    file_depth = do File {} <- idR 
                    return $ maxi (1::Integer)


-- Histogram

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

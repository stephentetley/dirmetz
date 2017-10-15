{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  MetricsLib.Base
-- Copyright   :  (c) Stephen Tetley 2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Datatypes and utilities for metrics.
--
--------------------------------------------------------------------------------


module MetricsLib.Base where

import Language.KURE                    -- package: kure

import Data.Int
import Data.Semigroup
import Data.Time


type ErrMsg = String

data Result a = Ans a
              | Err ErrMsg
  deriving (Eq,Ord,Show)

-- Push metrics failing up to the final answer...
--
toResult :: Either String a -> Result a
toResult (Left err) = Err err
toResult (Right a)  = Ans a

runKureResultM :: KureM a -> Result a
runKureResultM = toResult . runKureM Right Left


data Metric a = Metric
    { metric_name       :: String
    , metric_desription :: String
    , metric_result     :: Result a
    }
  deriving (Eq,Ord,Show)


type FormatedMetric = Metric String



type Maxi = Max Int64

maxi :: Integral a => a -> Maxi
maxi = Max . fromIntegral





-- Probably call MaxInteger LargestInteger.
-- There is scope for max-min Ints, doubles, dates...


newtype LargestInteger = LargestInteger { getLargestInteger :: Max Integer }
  deriving (Eq,Ord,Show,Read)

instance Monoid LargestInteger where
  mempty = LargestInteger 0 
  LargestInteger i1 `mappend` LargestInteger i2 = LargestInteger $ i1 <> i2



newtype SmallestInteger = SmallestInteger { getSmallestInteger :: Min Integer }
  deriving (Eq,Ord,Show,Read)

-- Make a semigroupo a monoid with a sentinel (-1).
--
instance Monoid SmallestInteger where
  mempty = SmallestInteger (-1) 
  a@(SmallestInteger i1) `mappend` b@(SmallestInteger i2) 
      | i1 < 0          = b     -- always go for b if a is the sentinel
      | i2 < i1         = b
      | otherwise       = a


{-
newtype Latest = Latest { getLatest :: Labelled UTCTime }
  deriving (Eq,Ord)

instance Monoid Latest where
  mempty = Latest $ Labelled "" undefined 
  a@(Latest i1) `mappend` b@(Latest i2) = if valueOf i1 >= valueOf i2 then a else b
-}


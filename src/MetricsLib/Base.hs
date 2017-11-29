{-# LANGUAGE LambdaCase                 #-}
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


-- Uminho metrics use Strings and Read to hide the representation of metrics
-- On groups, Uminho metrics calculates aggregate min, max and ave.
-- However I don't know that this is a design I want to follow - 
-- min, max, ave seem relevant to size metrics

type FormattedMetric = Metric String

data GroupMetric = GroupMetric String [FormattedMetric]



type Maxi = Max Int64

maxi :: Integral a => a -> Maxi
maxi = Max . fromIntegral





-- Probably call MaxInteger LargestInteger.
-- There is scope for max-min Ints, doubles, dates...


newtype LargestInteger = LargestInteger { getLargestInteger :: Max Integer }
  deriving (Eq,Ord,Show,Read)

instance Monoid LargestInteger where
  mempty = LargestInteger (-1)
  LargestInteger i1 `mappend` LargestInteger i2 = LargestInteger $ i1 <> i2



newtype SmallestInteger = SmallestInteger { getSmallestInteger :: Min Integer }
  deriving (Eq,Ord,Show,Read)



-- Make a semigroup a monoid with a sentinel (-1).
--
instance Monoid SmallestInteger where
  mempty = SmallestInteger (-1) 
  a@(SmallestInteger i1) `mappend` b@(SmallestInteger i2) 
      | i1 < 0          = b     -- always go for b if a is the sentinel
      | i2 < i1         = b
      | otherwise       = a



maybeMappendBy :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
maybeMappendBy fn = step
  where
    step Nothing  b         = b
    step a        Nothing   = a
    step (Just a) (Just b)  = Just $ fn a b

swapMaybeT :: Monad m => String -> Transform c m a (Maybe b) -> Transform c m a b
swapMaybeT errmsg ma  = transform $ \c a -> applyT ma c a >>= post
  where
    post (Just a) = return a
    post Nothing  = fail errmsg


-- This is swapMaybeT for numeric types where a negative number 
-- is the failure sentinel.
positiveT :: (Monad m, Num b, Ord b) => String -> Transform c m a b -> Transform c m a b
positiveT errmsg ma  = transform $ \c a -> applyT ma c a >>= post
  where
    post a | a >= 0    = return a
           | otherwise = fail errmsg


newtype Latest = Latest { getLatest :: Maybe UTCTime }
  deriving (Eq,Ord,Show,Read)

instance Monoid Latest where
  mempty = Latest Nothing
  Latest a `mappend` Latest b = Latest $ maybeMappendBy fn a b
    where
      fn d1 d2 = if d1 >= d2 then d1 else d2



newtype Earliest = Earliest { getEarliest :: Maybe UTCTime }
  deriving (Eq,Ord,Show,Read)

instance Monoid Earliest where
  mempty = Earliest Nothing
  Earliest a `mappend` Earliest b = Earliest $ maybeMappendBy fn a b
    where
      fn d1 d2 = if d1 >= d2 then d1 else d2



--------------------------------------------------------------------------------
-- Size

-- Size can be a multi-metric that tracks min, max and average.
-- This avoids repeated traversals for largest, smallest, average.

data SizeMetric a = SizeMetric 
    { size_minimum      :: !a
    , size_maximum      :: !a
    , size_average      :: !Double
    }
  deriving (Eq,Show,Read)
 

measure1ToMetric :: Real a => SizeMeasure1 a -> SizeMetric a
measure1ToMetric sm = 
    SizeMetric { size_minimum = getMin $ size_min sm
               , size_maximum = getMax $ size_max sm
               , size_average = realToFrac (size_sum sm) / realToFrac (size_count sm) 
               }



newtype SizeMeasure a = SizeMeasure { getSizeMeasure :: Maybe (SizeMeasure1 a) }
  deriving (Eq,Ord,Show,Read)

instance (Num a, Ord a) => Monoid (SizeMeasure a) where
  mempty = SizeMeasure Nothing
  SizeMeasure a `mappend` SizeMeasure b = SizeMeasure $ maybeMappendBy (<>) a b



sizeMeasure :: Num a => a -> SizeMeasure a
sizeMeasure i = SizeMeasure $ Just $ sizeMeasure1 i



data SizeMeasure1 a = SizeMeasure1
    { size_sum      :: !a
    , size_count    :: !Integer
    , size_min      :: Min a
    , size_max      :: Max a
    }
  deriving (Eq,Ord,Show,Read)


instance (Num a, Ord a) => Semigroup (SizeMeasure1 a) where
  a <> b = SizeMeasure1 { size_sum      = size_sum a + size_sum b
                        , size_count    = size_count a + size_count b
                        , size_min      = size_min a <> size_min b
                        , size_max      = size_max a <> size_max b }


sizeMeasure1 :: Num a => a -> SizeMeasure1 a
sizeMeasure1 i = 
    SizeMeasure1 { size_sum      = i
                 , size_count    = 1
                 , size_min      = Min i
                 , size_max      = Max i }



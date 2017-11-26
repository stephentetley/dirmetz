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


{-

-- Workings out to get swapMaybeT

listFirst1 :: Transform c Maybe [b] b
listFirst1 = transform $ \c a -> case a of
  [] -> Nothing
  (a:_) -> Just a

listFirst2 :: Monad m => Transform c m [b] (Maybe b)
listFirst2 = transform $ \c a -> case a of
  [] -> return $ Nothing
  (a:_) -> return $ Just a

swapMaybeR1 :: Monad m => String -> Rewrite c m a -> Rewrite c m a
swapMaybeR1 errmsg ma  = rewrite $ \c a -> post (applyR ma c a)
  where
    post _ = fail errmsg



swap1 :: Rewrite c m a -> Rewrite c m a
swap1 ma  = rewrite $ \c a -> post (applyR ma c a)
  where
    post a = a
-}       

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




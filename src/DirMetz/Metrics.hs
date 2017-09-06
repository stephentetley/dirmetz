{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  DirMetz.Metrics
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


module DirMetz.Metrics where

import Data.Semigroup
import Data.Time


-- labelling Metrics seems like a good idea

data Labelled a = Labelled !String !a 
  deriving (Eq,Ord,Show,Read)

valueOf :: Labelled a -> a
valueOf (Labelled _ a) = a

labelOf :: Labelled a -> String
labelOf (Labelled s _) = s


-- Probably call MaxInteger LargestInteger.
-- There is scope for max-min Ints, doubles, dates...


newtype LargestInteger = LargestInteger { getLargestInteger :: Labelled Integer }
  deriving (Eq,Ord,Show,Read)

instance Monoid LargestInteger where
  mempty = LargestInteger $ Labelled "" 0 
  a@(LargestInteger i1) `mappend` b@(LargestInteger i2) = if valueOf i1 >= valueOf i2 then a else b



newtype SmallestInteger = SmallestInteger { getSmallestInteger :: Labelled Integer }
  deriving (Eq,Ord,Show,Read)

instance Monoid SmallestInteger where
  mempty = SmallestInteger $ Labelled "" (-1) 
  a@(SmallestInteger i1) `mappend` b@(SmallestInteger i2) 
      | valueOf i1 < 0          = b     -- always go for b if a is the sentinel
      | valueOf i2 < valueOf i1 = b
      | otherwise               = a



newtype Latest = Latest { getLatest :: Labelled UTCTime }
  deriving (Eq,Ord)

instance Monoid Latest where
  mempty = Latest $ Labelled "" undefined 
  a@(Latest i1) `mappend` b@(Latest i2) = if valueOf i1 >= valueOf i2 then a else b


-- TODO 
-- Metrics probably warrant a strict Maybe type then we have a 
-- clearly represented unknown initial value (and don't have to
-- worry about negative numbers).
--
-- Counting / summing metrics are a different matter and can 
-- always start from zero
--

data Value a = NoValue
             | Some !a
  deriving (Eq,Ord,Show,Read)

instance Semigroup a => Monoid (Value a) where
   mempty = NoValue
   NoValue  `mappend` b               = b
   a              `mappend` NoValue   = a
   Some a         `mappend` Some b          = Some $ a <> b

appendWith :: (a -> a -> a) -> Value a -> Value a -> Value a
appendWith _  NoValue   b           = b
appendWith _  a         NoValue     = a
appendWith op (Some a)  (Some b)    = Some $ a `op`  b

   
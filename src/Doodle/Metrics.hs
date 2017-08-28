{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Doodle.Metrics
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


module Doodle.Metrics where





-- labelling Metrics seems like a good idea

data Labelled a = Labelled !String !a 
  deriving (Eq,Ord,Show,Read)

valueOf :: Labelled a -> a
valueOf (Labelled _ a) = a

labelOf :: Labelled a -> String
labelOf (Labelled s _) = s


-- Probably call MaxInteger LargestInteger.
-- There is scope for max-min Ints, doubles, dates...


newtype MaxInteger = MaxInteger { getMaxInteger :: Labelled Integer }
  deriving (Eq,Ord,Show,Read)

instance Monoid MaxInteger where
  mempty = MaxInteger $ Labelled "" 0 
  a@(MaxInteger i1) `mappend` b@(MaxInteger i2) = if valueOf i1 >= valueOf i2 then a else b


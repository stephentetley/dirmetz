{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  DirMetz.FileSys.RegexGlob
-- Copyright   :  (c) Stephen Tetley 2018
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Globbing via translation to Regex.
--
-----------------------------------------------------------------------------


module DirMetz.FileSys.RegexGlob where

import Data.Maybe

import Text.Regex

translate :: String -> String
translate ss = unwrap $ showChar '^' . work ss . showChar '$'
  where
    unwrap = ($ "")
    body = id

    specials      = "\\+()^$.{}]|"

    work ""         = id
    work ('*':cs)   = showString ".*" . work cs
    work ('?':cs)   = showString "." . work cs
    
    work (c:cs)     = escape c . work cs

    escape c | c `elem` specials    = showChar '\\' . showChar c
             | otherwise            = showChar c

match1 :: String -> String -> Bool
match1 patt input = 
   let regex = mkRegex $ translate patt in isJust $ matchRegex regex input

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
-- Acknowledgement - this largely taken from 
-- Real World Haskell - Chapter 8 
-- Bryan O'Sullivan, John Goerzen & Don Stewart
--
-----------------------------------------------------------------------------


module DirMetz.FileSys.RegexGlob where


import Text.Regex                -- package: regex-compat

import System.FilePath
import Data.Maybe

translate :: String -> String
translate ss = unwrap $ showChar '^' . work ss . showChar '$'
  where
    unwrap              = ($ "")

    specials            = "\\+()^$.{}]|"

    work []             = id
    work ('*':cs)       = showString ".*" . work cs
    work ('?':cs)       = showString "." . work cs
    work ('[':'!':cs)   = showString "[^" . charClass cs
    work ('[':cs)       = showChar '[' . charClass cs
    
    work (c:cs)         = escape c . work cs

    
    charClass (']':cs) = showChar ']' . work cs
    charClass (c:cs)   = showChar c . charClass cs
    charClass []       = error $ "Invalid glob char-class"

    escape c | c `elem` specials    = showChar '\\' . showChar c
             | otherwise            = showChar c

match1 :: String -> String -> Bool
match1 patt input = 
   let regex = mkRegex $ translate patt in isJust $ matchRegex regex input


isPatt :: String -> Bool
isPatt = any (`elem` "[*?")
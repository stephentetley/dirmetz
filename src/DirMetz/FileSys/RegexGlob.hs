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

import DirMetz.FileSys.Base

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


isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMatching :: String -> FileStore -> [String]
namesMatching pat store
    | not (isPattern pat) = 
        if doesPathExist pat store then [pat] else []
    | otherwise           = 
        case splitFileName pat of
            ("",baseName) -> listMatches "" baseName store
            (dirName,baseName) -> error "TODO"
                   

listMatches :: FilePath -> String -> FileStore -> [String]
listMatches dirName pat store = 
    checkHiddens $ getDirectoryContents dirName store
  where
    -- dirName1 = if null dirName then "" else dirName
    checkHiddens names
        | isHidden pat = filter isHidden names
        | otherwise    = filter (not . isHidden) names 
                 

isHidden :: String -> Bool
isHidden ('.':_) = True
isHidden _       = False

listPlain :: FilePath -> String -> FileStore -> [String]
listPlain dirName baseName store = 
    if exists then [baseName] else []
  where
    exists = if null baseName 
             then doesFolderExist dirName store
             else doesPathExist (dirName </> baseName) store

               
          
      
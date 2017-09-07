{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  DirMetz.FileSys.DirRecurseParser
-- Copyright   :  (c) Stephen Peter Tetley 2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  to be determined
--
-- Parser for output from Windows Powershell @dir -recurse@ command.
--
-----------------------------------------------------------------------------


module DirMetz.FileSys.DirRecurseParser where



import Text.Parsec                              -- package: parsec

import Control.Monad.Identity                   -- package: mtl

type ParsecParser a        = ParsecT String () Identity a
-- type ParsecLexer           = GenTokenParser String () Identity

pMode :: ParsecParser String
pMode = many1 (lower <|> char '-')
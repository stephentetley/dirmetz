{-# OPTIONS -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  DirMetz.FileSys.DirRecurseParser
-- Copyright   :  (c) Stephen Tetley 2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Parser for output from Windows Powershell @dir -recurse@ command.
--
-----------------------------------------------------------------------------


module DirMetz.FileSys.DirRecurseParser where

import DirMetz.FileSys.Base

import Text.Parsec                              -- package: parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P


import Control.Monad.Identity                   -- package: mtl

import Data.Time                                -- pacake: Time

import Data.Bifunctor
import Data.Char

type Parser a        = ParsecT String () Identity a
type Lexer           = P.GenTokenParser String () Identity

testP :: Parser a -> String -> Either String a
testP p s = first show $ parse p "no source" s 
      
lineEnd :: Parser ()
lineEnd = void endOfLine <|> eof

lineOf :: Parser a -> Parser a
lineOf p = p <* lineEnd

blankLine :: Parser ()
blankLine = spaces *> lineEnd

pMode :: Parser String
pMode = many1 (lower <|> char '-')

pName :: Parser String
pName = trimr <$> manyTill anyChar lineEnd


-- TODO can Data.Time.Format help us from being UK biased...
-- WARNING UK Biased
pDate :: Parser Day
pDate = (\d m y ->  fromGregorian y m d) 
            <$> (int <* char '/') <*> (int <* char '/') <*> integer 
      
pTime :: Parser DiffTime
pTime = (\h m -> hoursMinutesSecondsToDiffTime h m 0)
            <$> (int <* char ':') <*> int


pUTCTime :: Parser UTCTime
pUTCTime = UTCTime <$> lexeme pDate <*> lexeme pTime

--------------------------------------------------------------------------------
-- Lexer and utils


int                 :: Parser Int
int                 = fromIntegral <$> P.integer lexer_def

integer             :: Parser Integer
integer             = P.integer lexer_def


lexeme              :: Parser a -> Parser a
lexeme              = P.lexeme lexer_def

lexer_def :: Lexer 
lexer_def = P.makeTokenParser emptyDef


trimr :: String -> String
trimr = foldr step []
  where
    step c [] | isSpace c = []
    step c ac             = c:ac
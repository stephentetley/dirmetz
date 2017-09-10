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



-- Use a notational convention - Directories are not "filled" until 
-- post-processing after parsing, hence use another name.
-- 
type Element = FileObj

data Block = Block Name Element
  deriving (Show)



pMode :: Parser String
pMode = many1 (lower <|> char '-')

pName :: Parser String
pName = trimr <$> manyTill anyChar lineEnd


-- TODO maybe Data.Time.Format can help us from being UK biased...
-- WARNING UK Biased
pDate :: Parser Day
pDate = (\d m y ->  fromGregorian y m d) 
            <$> (int <* char '/') <*> (int <* char '/') <*> integer 
      
pTime :: Parser DiffTime
pTime = (\h m -> hoursMinutesSecondsToDiffTime h m 0)
            <$> (int <* char ':') <*> int


pUTCTime :: Parser UTCTime
pUTCTime = UTCTime <$> lexeme pDate <*> lexeme pTime

pLastWriteTime :: Parser Properties 
pLastWriteTime = 
    (\modtime -> Properties { access_time = Nothing
                            , modification_time = Just modtime })
                <$> pUTCTime
  

pSize :: Parser Size
pSize = integer

pDirectoryName :: Parser String
pDirectoryName = spaces *> string "Directory:" *> spaces *> pName

pHeadings :: Parser ()
pHeadings = titles *> underlines
  where
    titles = sepStrings ["Mode", "LastWriteTime", "Length", "Name"]  
               *> ws *> endOfLine 
    underlines = (underline *> ws) *> (underline *> ws) *>
                 (underline *> ws) *> (underline *> ws) *> lineEnd


pElement :: Parser Element
pElement = pMode >>= elementK
  where
    elementK mode | isDir mode  = (\props name -> Folder name props [])
                                    <$> pLastWriteTime <*> pName
    
    elementK _                  = (\props size name -> File name props size)
                                    <$> pLastWriteTime <*> pSize <*> pName

--------------------------------------------------------------------------------
-- Lexer and utils

sepStrings :: [String]  -> Parser [String]
sepStrings []           = return []
sepStrings (s:ss)       = step s ss
  where
    step :: String -> [String] -> Parser [String]
    step s1 []           = (\x -> [x]) <$> string s1
    step s1 (c:cs)       = (:) <$> (string s1 <* spaces) <*> step c cs



ws :: Parser () 
ws = many (char ' ' <|> char '\t') *> return ()

ws1 :: Parser () 
ws1 = many1 (char ' ' <|> char '\t') *> return ()

lineEnd :: Parser ()
lineEnd = void endOfLine <|> eof

lineOf :: Parser a -> Parser a
lineOf p = p <* lineEnd

blankLine :: Parser ()
blankLine = spaces *> lineEnd

underline :: Parser ()
underline = many1 (char '-') *> return ()


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


isDir :: String -> Bool
isDir ('d':_)   = True
isDir _         = False
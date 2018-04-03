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
import Text.Parsec.Language ( emptyDef )
import Text.Parsec.String ( parseFromFile )
import qualified Text.Parsec.Token as P


import Control.Monad.Identity                   -- package: mtl

import Data.Time                                -- pacake: Time

import Data.Bifunctor
import Data.Char
import qualified Data.Map.Strict as MAP


type Parser a        = ParsecT String () Identity a
type Lexer           = P.GenTokenParser String () Identity

testP :: Parser a -> String -> Either String a
testP p s = first show $ parse p "no source" s 

testIOP :: Parser a -> FilePath -> IO (Either String a)
testIOP p path = fmap (first show) $ parseFromFile p path




-- Use a notational convention - Directories are not "filled" until 
-- post-processing after parsing, hence use another name.
-- 

-- TODO should have a distinct syntax where Element/Folders are 
-- not recursive

type Element = Content
type Mode = String

{-
-- TODO
data Element_ = WinFile   Mode UTCTime Size Name
              | WinFolder Mode UTCTime Name 

-}

data Block = Block Name [Element]
  deriving (Show)

type Listing = [Block]

pMode :: Parser String
pMode = many1 (lower <|> char '-')



pName :: Parser String
pName = trimr <$> manyTill anyChar (try lineEnd)


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


-- | Note - a long name is printed on the next line.
--
pDirectoryName :: Parser String
pDirectoryName = string "Directory:" *> ws1 *> rest
  where
    rest = (lineEnd *> pName) <|> pName

pHeaderLines :: Parser ()
pHeaderLines = titles *> underlines
  where
    titles = sepStrings ["Mode", "LastWriteTime", "Length", "Name"]  
               *> ws *> endOfLine 
    underlines = (underline *> ws) *> (underline *> ws) *>
                 (underline *> ws) *> (underline *> ws) *> lineEnd



-- | This is a context sensitive parser 
--
pElement :: Parser Element
pElement = pMode >>= elementK
  where
    elementK mode | isDir mode  = (\props name -> FsFolder name props [])
                                    <$> pLastWriteTime <*> pName
    
    elementK _                  = (\props size name -> FsFile name props size)
                                    <$> pLastWriteTime <*> pSize <*> pName

pBlock :: Parser Block
pBlock = Block  <$> indented pDirectoryName
                <*> (twice blankLine *> pHeaderLines *> many pElement)
                <*  twice blankLine
  <?> "BLOCK"

pListing :: Parser Listing
pListing = twice emptyLine *> many1 pBlock
        <?> "LISTING"


-- Problem - output of dir -recurse seems to be in "UCS-2 LE BOM"
-- and not UTF8, this is causing horrible errors...
--
readListing :: FilePath -> IO (Either String Content)
readListing path = do 
    ans <- parseFromFile pListing path
    return $ case ans of 
       Left err -> Left $ show err
       Right ls -> case buildTopDown ls of
             Nothing -> Left $ "Could not build root file object"
             Just a -> Right $ a

--------------------------------------------------------------------------------
-- Lexer and utils

twice :: Parser a -> Parser (a,a)
twice p = (,) <$> p <*> p

threeTimes :: Parser a -> Parser (a,a,a)
threeTimes p = (,,) <$> p <*> p <*> p

sepStrings :: [String]  -> Parser [String]
sepStrings []           = return []
sepStrings (s:ss)       = step s ss
  where
    step :: String -> [String] -> Parser [String]
    step s1 []           = (\x -> [x]) <$> string s1
    step s1 (c:cs)       = (:) <$> (string s1 <* ws1) <*> step c cs


indented :: Parser a -> Parser a
indented p = ws1 *> p


ws :: Parser () 
ws = many (char ' ' <|> char '\t') *> return ()

ws1 :: Parser () 
ws1 = many1 (char ' ' <|> char '\t') *> return ()

lineEnd :: Parser ()
lineEnd = void endOfLine <|> eof
  where
    -- Something has gone wrong with CRLF detection...
    --    winEnd = void $ (char (chr 160) <|> char (chr 9632))

lineOf :: Parser a -> Parser a
lineOf p = p <* lineEnd

blankLine :: Parser ()
blankLine = ws *> lineEnd

emptyLine :: Parser ()
emptyLine = lineEnd


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

--------------------------------------------------------------------------------
-- Build from flat.


buildTopDown :: Listing -> Maybe Content
buildTopDown listing = fmap build1 $ getRoot listing
  where
    root_props = Properties { access_time = Nothing
                            , modification_time = Nothing }
    
    -- Note - Root has no properties, so we can't make with makeRecur
    --
    build1 (Block name es) = let level1_kids = makeLevel1Kids listing
                                 kids = map (makeRecur level1_kids name) es
                             in FsFolder name root_props kids

-- | Root is always first
getRoot :: Listing -> Maybe Block
getRoot (x:_) = Just x
getRoot []    = Nothing


makeRecur :: Level1Kids -> Name -> Element -> Content
makeRecur _     _       obj@(FsFile {})               = obj
makeRecur store parent      (FsFolder name2 props _)  = 
   let fullname = parent ++ ('\\':name2)
       kids1    = MAP.findWithDefault [] fullname store
       kids2    = map (makeRecur store fullname) kids1
   in FsFolder name2 props kids2


type Level1Kids = MAP.Map Name [Content]

makeLevel1Kids :: Listing -> Level1Kids
makeLevel1Kids = foldr step MAP.empty
  where
    step (Block name es) m = MAP.insert name es m
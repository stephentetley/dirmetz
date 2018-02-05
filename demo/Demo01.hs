{-# OPTIONS -Wall #-}


module Demo01 where


import DirMetz.FileSys.Base
import DirMetz.FileSys.DirRecurseParser
import DirMetz.FileSys.Kure hiding ( blankLine )
import DirMetz.FileSys.Metrics1
import DirMetz.FileSys.RegexGlob

import MetricsLib.Base

-- import Language.KURE                    -- package: kure

import Data.Time
import System.Directory


--------------------------------------------------------------------------------
-- Demos

demo01 :: IO ()
demo01 = getCurrentDirectory >>= populate >>= print

demo02 :: IO ()
demo02 = getCurrentDirectory >>= populate >>= \fo -> case prettyPrint fo of
    Right ans -> print ans
    Left err -> error err



demo03 :: IO ()
demo03 = getCurrentDirectory >>= populate >>= \fo -> case calcLargestFile fo of
    Ans ans -> print ans
    Err err -> error err



demo04 :: IO ()
demo04 = getCurrentDirectory >>= populate >>= \fo -> case (calcCountFiles fo, calcCountFolders fo) of
    (Ans i1, Ans i2) -> print $ (i1,i2)
    (Err err, _) -> error err
    (_, Err err) -> error err


demo05 :: IO ()
demo05 = getCurrentDirectory >>= populate >>= \fo -> case calcMaxDepth fo of
    Ans ans -> print ans
    Err err -> error err

demo06 :: IO ()
demo06 = getCurrentDirectory >>= populate >>= \fo -> case subsystems fo of
    Right ans -> print ans
    Left err -> error err


demo07 = readListing "./demo/data/dir-recurse.txt"
demo08 = readListing "./demo/data/dir-recurse.txt"


-- demo08 :: IO ()
-- demo08 = getCurrentDirectory >>= populate >>= \fo -> putStrLn (display fo) 

demo09 :: IO ()
demo09 = getCurrentDirectory >>= populateFS >>= \fo ->  putStrLn (display fo) 


-- getModificationTime returns UTCTime 
type Timestamp = UTCTime

temp01 :: IO Timestamp
temp01 = getCurrentTime

temp02 :: UTCTime
temp02 = UTCTime { utctDay = day, utctDayTime = daytime }
  where
    day = fromGregorian 2017 09 05
    daytime = hoursMinutesSecondsToDiffTime 12 30 00


temp03 = testP pName "File Name with space.txt     "
temp04 = testP pUTCTime "29/05/2017     15:27      "
temp05 = testP lineEnd ""
temp06 = testIOP (lineEnd *> lineEnd *> pDirectoryName) "./demo/data/dir-recurse.txt"

temp07 = testP ((,) <$> dname <*> headers) $ 
       unlines $ [ ""
                 , ""
                 , "    Directory: E:\\coding\\fsharp\\fsharp-snippets"
                 , ""
                 , ""
                 , "Mode                LastWriteTime         Length Name                                                                  "
                 , "----                -------------         ------ ----                                                                  "
                  ,""
                 ]
  where
    dname = emptyLine *> emptyLine *> indented pDirectoryName
    headers = emptyLine *> emptyLine *> pHeaderLines


temp08a = match1 "*.doc" "e:/documents/elephant.doc"
temp08b = match1 "*.doc" "e:/documents/elephant.xls"

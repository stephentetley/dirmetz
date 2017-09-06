{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS -Wall #-}


module DemoFileListing where

import Doodle.FileSys.Base
import Doodle.FileSys.Kure

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
demo03 = getCurrentDirectory >>= populate >>= \fo -> case largestFile fo of
    Right ans -> print ans
    Left err -> error err

demo03a :: IO ()
demo03a = getCurrentDirectory >>= populate >>= \fo -> case largestFileNamed fo of
    Right ans -> print ans
    Left err -> error err

demo04 :: IO ()
demo04 = getCurrentDirectory >>= populate >>= \fo -> case (countFiles fo, countFolders fo) of
    (Right i1, Right i2) -> print $ (i1,i2)
    (Left err, _) -> error err
    (_, Left err) -> error err


demo05 :: IO ()
demo05 = getCurrentDirectory >>= populate >>= \fo -> case maxDepth fo of
    Right ans -> print ans
    Left err -> error err

demo06 :: IO ()
demo06 = getCurrentDirectory >>= populate >>= \fo -> case subsystems fo of
    Right ans -> print ans
    Left err -> error err


-- getModificationTime returns UTCTime 
type Timestamp = UTCTime

temp01 :: IO Timestamp
temp01 = getCurrentTime

temp02 :: UTCTime
temp02 = UTCTime { utctDay = day, utctDayTime = daytime }
  where
    day = fromGregorian 2017 09 05
    daytime = hoursMinutesSecondsToDiffTime 12 30 00

hoursMinutesSecondsToDiffTime :: Int -> Int -> Integer -> DiffTime
hoursMinutesSecondsToDiffTime h m s = secondsToDiffTime $ s + 60 * (fromIntegral m) + 3600 * (fromIntegral h)
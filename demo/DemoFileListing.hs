{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS -Wall #-}


module DemoFileListing where

import Doodle.FileSys.Base

-- import Language.KURE                    -- package: kure


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

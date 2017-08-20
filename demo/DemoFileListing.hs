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


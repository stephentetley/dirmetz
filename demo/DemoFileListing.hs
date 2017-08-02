{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# OPTIONS -Wall #-}


module DemoFileListing where



import Language.KURE


type Name = String

data FileObj = File Name
             | Folder Name [FileObj]
  deriving (Show)


data U = UFO FileObj

instance Injection FileObj U where
  inject = UFO
  project (UFO fo) = Just fo



instance Walker cx U where
  allR :: MonadCatch m => Rewrite c m U -> Rewrite c m U
  allR r = prefixFailMsg "allR failed: " $
           rewrite $ \cx -> \(UFO fo) -> inject <$> applyR allRfileObj cx fo
    where
      allRfileObj :: Monad m => Rewrite c m FileObj
      allRfileObj = readerT $ \case 
                      File {} -> idR
                      Folder {} -> undefined

--      allFileObj cx (File s) = File <$> pure s
--      allFileObj cx (Folder s fs) = undefined -- let fn = extractR r in Folder <$> pure s <*> mapM (fn cx) fs

foldersAllR :: Monad m => Rewrite c m [FileObj] -> Rewrite c m [FileObj] -> Rewrite c m [FileObj]

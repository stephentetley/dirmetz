{-# LANGUAGE MultiParamTypeClasses      #-}
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


instance Walker U where
  allR r = \(UFO fo) -> UFO <$> allFileObj fo
    where
      allFileObj (File s) = File <$> pure s
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


-- Note the Lam example distributed with KURE implies we don't 
-- need a universe as we have a single type

data U = UFO FileObj

instance Injection FileObj U where
  inject = UFO
  project (UFO fo) = Just fo


-- Congruence combinator                     
fileT :: Monad m => (Name -> b) -> Transform c m FileObj b
fileT f = contextfreeT $ \case
    File s -> return (f s)
    _     -> fail "not a File"


-- congruence combinator
-- Note Path is not propagated (this is an error)
folderT :: Monad m 
        => Transform c m FileObj a -> (Name -> [a] -> b) -> Transform c m FileObj b
folderT t f = transform $ \c -> \case
    Folder s ks -> f s <$> mapM (\fo -> applyT t c fo) ks
    _ -> fail "not a Folder"
                             

folderAllR :: Monad m => Rewrite c m FileObj -> Rewrite c m FileObj
folderAllR r = folderT r Folder



instance Walker cx U where
  allR :: MonadCatch m => Rewrite c m U -> Rewrite c m U
  allR r = prefixFailMsg "allR failed: " $
           rewrite $ \cx -> \(UFO fo) -> inject <$> applyR allRfileObj cx fo
    where
      allRfileObj = readerT $ \case 
                      File {} -> idR
                      Folder {} -> folderAllR (extractR r)

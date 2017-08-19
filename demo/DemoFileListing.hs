{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# OPTIONS -Wall #-}


module DemoFileListing where



import Language.KURE                    -- package: kure

import Text.PrettyPrint                 -- package: pretty

import Control.Monad
import System.Directory
import System.FilePath



type Name = String

data FileObj = File Name
             | Folder Name [FileObj]
  deriving (Show)


-- Note the Lam example distributed with KURE implies we don't 
-- need a universe as we have a single type
-- Also if were read Context as the analogue to inherited 
-- attributes in attribute grammars, (parent) path would be an 
-- obvious context.


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
  allR :: MonadCatch m => Rewrite cx m U -> Rewrite cx m U
  allR r = prefixFailMsg "allR failed: " $
           rewrite $ \cx -> \(UFO fo) -> inject <$> applyR allRfileObj cx fo
    where
      allRfileObj = readerT $ \case 
                      File {} -> idR
                      Folder {} -> folderAllR (extractR r)


--------------------------------------------------------------------------------
-- Populate 

populate :: FilePath -> IO FileObj
populate = foldersR
  where
    listDirectoryLong :: FilePath -> IO [FilePath]
    listDirectoryLong path = map (path </>) <$> listDirectory path

    foldersR path = do { kids <- filterM doesDirectoryExist =<< listDirectoryLong path 
                       ; kids' <- mapM foldersR kids
                       ; files <- files1 path
                       ; return $ Folder path (kids' ++ files) }
                       
    files1 path = do { xs <- filterM doesFileExist =<< listDirectoryLong path 
                     ; return $ map (File . takeFileName) xs }


--------------------------------------------------------------------------------
-- Pretty print

type Context = ()

type RewriteE a     = Rewrite Context KureM a
type TransformE a b = Transform Context KureM a b


-- allT is onelayer traversal
{-
prettyPrint :: FileObj -> Doc
prettyPrint fo = top $+$ rest
  where
    top = vsep $ applyT (allT pretty1) fo
    rest = text "..."
-}

prettyPrint :: FileObj -> Either String Doc
prettyPrint = fmap vsep . runKureM Right Left . applyT prettyDir () . UFO


-- Ideally KURE would have a one-level version of collectT 
prettyDir :: TransformE U [Doc]
prettyDir = withPatFailMsg "addLitR failed" $
            do (UFO (Folder {})) <- idR
               collectPruneT prettyG1



vsep :: [Doc] -> Doc
vsep []         = empty
vsep [d]        = d
vsep (d:ds)     = d $+$ vsep ds

prettyG1 :: TransformE U Doc
prettyG1 = promoteT pretty1

-- No descending into terms.
pretty1 :: TransformE FileObj Doc
pretty1 = transform $ \_ -> \case
    File s -> return $ nest 6 (text s)
    Folder s _ -> return $ text "<DIR>" <+> text s


--------------------------------------------------------------------------------
-- Demos

demo01 :: IO ()
demo01 = getCurrentDirectory >>= populate >>= print

demo02 :: IO ()
demo02 = getCurrentDirectory >>= populate >>= \fo -> print (prettyPrint fo) 
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




-- Congruence combinator                     
fileT :: Monad m => (Name -> b) -> Transform c m FileObj b
fileT f = contextfreeT $ \case
    File s -> return (f s)
    _     -> fail "not a File"

-- congruence combinator
-- Note Path is not propagated (this is a limitation that could be improved)
folderT :: Monad m 
        => Transform c m FileObj a -> (Name -> [a] -> b) -> Transform c m FileObj b
folderT t f = transform $ \c -> \case
    Folder s ks -> f s <$> mapM (\fo -> applyT t c fo) ks
    _ -> fail "not a Folder"
                             

folderAllR :: Monad m => Rewrite c m FileObj -> Rewrite c m FileObj
folderAllR r = folderT r Folder



instance Walker cx FileObj where
  allR :: MonadCatch m => Rewrite cx m FileObj -> Rewrite cx m FileObj
  allR r = prefixFailMsg "allR failed: " $
           rewrite $ \cx fo -> inject <$> applyR allRfileObj cx fo
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
prettyPrint = fmap getLineDoc . runKureM Right Left . applyT prettyDir ()


-- Ideally KURE would have a one-level version of collectT 
prettyDir :: TransformE FileObj LineDoc
prettyDir = withPatFailMsg "addLitR failed" $
            do Folder {} <- idR
               d1 <- allT pretty1
               ds <- allT (mtryM prettyDir)
               return $ d1 `mappend` ds

newtype LineDoc = LineDoc { getLineDoc :: Doc }

instance Monoid LineDoc where
  mempty = LineDoc  empty
  a `mappend` b = LineDoc $ getLineDoc a $+$ getLineDoc b


vsep :: [Doc] -> Doc
vsep []         = empty
vsep [d]        = d
vsep (d:ds)     = d $+$ vsep ds


-- No descending into terms.
pretty1 :: TransformE FileObj LineDoc
pretty1 = transform $ \_ -> \case
    File s -> return $ LineDoc $ nest 6 (text s)
    Folder s _ -> return $ LineDoc $ text "<DIR>" <+> text s


--------------------------------------------------------------------------------
-- Demos

demo01 :: IO ()
demo01 = getCurrentDirectory >>= populate >>= print

demo02 :: IO ()
demo02 = getCurrentDirectory >>= populate >>= \fo -> print (prettyPrint fo) 
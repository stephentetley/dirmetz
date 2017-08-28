{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Doodle.FileSys.Base
-- Copyright   :  (c) Stephen Tetley 2017
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Generic traversals for External syntax.
--
--------------------------------------------------------------------------------


module Doodle.FileSys.Base where

import Doodle.Metrics

import Language.KURE                    -- package: kure

import Text.PrettyPrint                 -- package: pretty

import Control.Monad
import Data.Int
import Data.Semigroup hiding ( (<>) )
import System.Directory
import System.FilePath



type Name = String
type Size = Integer

data FileObj = File   Name Size
             | Folder Name [FileObj]
  deriving (Show)

-- Other possible file stats are modification time, access time, premissions


-- Note the Lam example distributed with KURE implies we don't 
-- need a universe as we have a single type
-- Also if were read Context as the analogue to inherited 
-- attributes in attribute grammars, (parent) path would be an 
-- obvious context.

newtype Context = Context { getContext :: FilePath }

zeroContext :: Context
zeroContext = Context ""

instance ExtendPath Context FilePath where
  (@@) (Context p1) p2 = Context $ p1 </> p2




-- Congruence combinator                     
fileT :: Monad m => (Name -> Size -> b) -> Transform c m FileObj b
fileT f = contextfreeT $ \case
    File s sz -> return (f s sz)
    _         -> fail "not a File"

-- congruence combinator
-- Note Path is not propagated (this is a limitation that could be improved)
folderT :: (ExtendPath c FilePath, Monad m) 
        => Transform c m FileObj a -> (Name -> [a] -> b) -> Transform c m FileObj b
folderT t f = transform $ \c -> \case
    Folder s ks -> let c1 = c @@ s in f s <$> mapM (\fo -> applyT t c1 fo) ks
    _ -> fail "not a Folder"
                             

folderAllR :: (ExtendPath c FilePath, Monad m) => Rewrite c m FileObj -> Rewrite c m FileObj
folderAllR r = folderT r Folder



instance (ExtendPath c FilePath) => Walker c FileObj where
  allR :: MonadCatch m => Rewrite c m FileObj -> Rewrite c m FileObj
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
                       ; return $ Folder (takeFileName path) (kids' ++ files) }
                       
    files1 path = do { xs <- filterM doesFileExist =<< listDirectoryLong path 
                     ; mapM (\x -> do {sz <- getFileSize x; return $ File (takeFileName x) sz }) xs }


--------------------------------------------------------------------------------
-- Pretty print


type RewriteE a     = Rewrite Context KureM a
type TransformE a b = Transform Context KureM a b



prettyPrint :: FileObj -> Either String Doc
prettyPrint = fmap getLineDoc . runKureM Right Left . applyT prettyDir zeroContext

newtype LineDoc = LineDoc { getLineDoc :: Doc }

blankLine :: LineDoc 
blankLine = LineDoc $ text ""

-- Ideally KURE would have a one-level version of collectT 
prettyDir :: TransformE FileObj LineDoc
prettyDir = withPatFailMsg "addLitR failed" $
            do (c, Folder s _) <- exposeT
               let d0 = LineDoc $ text (getContext c </> s) <> char ':'
               d1 <- allT pretty1
               ds <- allT (mtryM prettyDir)
               return $ d0 `mappend` d1 `mappend` ds `mappend` blankLine


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
    File s _ -> return $ LineDoc $ nest 6 (text s)
    Folder s _ -> return $ LineDoc $ text "<DIR>" <+> text s

--------------------------------------------------------------------------------
-- Towards metrics

type Maxi = Max Int64

maxi :: Integral a => a -> Maxi
maxi = Max . fromIntegral

largestFile :: FileObj -> Either String Integer
largestFile = runKureM Right Left . applyT largestFile1 zeroContext


-- max monoid
largestFile1 :: TransformE FileObj Integer
largestFile1 = fmap (fromIntegral . getMax) $ crushtdT $ 
    do File _ sz <- idR
       return $ maxi sz



largestFileNamed :: FileObj -> Either String (String,Integer)
largestFileNamed = runKureM Right Left . applyT largestFileNamed1 zeroContext


-- max monoid
largestFileNamed1 :: TransformE FileObj (String,Integer)
largestFileNamed1 = fmap get $ crushtdT $ 
    do File s sz <- idR
       return $ MaxInteger $ Labelled s sz
  where
    get (MaxInteger (Labelled s i)) = (s,i)

{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Doodle.FileSys.Kure
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


module Doodle.FileSys.Kure where

import Doodle.FileSys.Base
import Doodle.Metrics


import Language.KURE                    -- package: kure

import Text.PrettyPrint                 -- package: pretty

-- import Data.Time                        -- package: time

import Data.Int
import qualified Data.Map as Map
import Data.Semigroup hiding ( (<>) )
import System.FilePath




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
fileT :: Monad m => (Name -> Properties -> Size -> b) -> Transform c m FileObj b
fileT f = contextfreeT $ \case
    File s props sz -> return (f s props sz)
    _         -> fail "not a File"

-- congruence combinator
-- Note Path is not propagated (this is a limitation that could be improved)
folderT :: (ExtendPath c FilePath, Monad m) 
        => Transform c m FileObj a -> (Name -> Properties -> [a] -> b) -> Transform c m FileObj b
folderT t f = transform $ \c -> \case
    Folder s props ks -> let c1 = c @@ s in f s props <$> mapM (\fo -> applyT t c1 fo) ks
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
            do (c, Folder s _ _) <- exposeT
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
    File s _ _ -> return $ LineDoc $ nest 6 (text s)
    Folder s _ _ -> return $ LineDoc $ text "<DIR>" <+> text s

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
    do File _ _ sz <- idR
       return $ maxi sz


-- Labelling means we can identify the largest node alongside its size.

largestFileNamed :: FileObj -> Either String (String,Integer)
largestFileNamed = runKureM Right Left . applyT largestFileNamed1 zeroContext


-- max monoid
largestFileNamed1 :: TransformE FileObj (String,Integer)
largestFileNamed1 = fmap get $ crushtdT $ 
    do File s _ sz <- idR
       return $ LargestInteger $ Labelled s sz
  where
    get (LargestInteger (Labelled s i)) = (s,i)


-- Counting 
-- Counting doesn't seem to need labelling unless we label the 
-- metric itself, although that seems unnecessary at the element 
-- level as the metric name never changes.



countFiles :: FileObj -> Either String Integer
countFiles = runKureM Right Left . applyT countFiles1 zeroContext


-- sum monoid
countFiles1 :: TransformE FileObj Integer
countFiles1 = fmap getSum $ crushtdT $ 
    do File {} <- idR
       return 1

countFolders :: FileObj -> Either String Integer
countFolders = runKureM Right Left . applyT countFolders1 zeroContext


-- Note - don't count the root folder (so don't use crushtdT)
--
-- Design note - this is implemented with a recursion that we 
-- could just as easily do without using strategies, can we do it better?
--
countFolders1 :: TransformE FileObj Integer
countFolders1 = fmap getSum go_kids
  where
    go_kids   = allT $ tryM 0 go_folder
    go_folder = do Folder {} <- idR
                   i <- go_kids
                   return $ 1 `mappend` i


-- deepest 
-- deepest is not a straight-forward topdown traversal


maxDepth :: FileObj -> Either String Integer
maxDepth = runKureM Right Left . applyT maxDepth1 zeroContext

maxDepth1 :: TransformE FileObj Integer
maxDepth1 = fmap (fromIntegral . getMax) $ allT $ folder_depth <+ file_depth 
  where
    folder_depth = do Folder {} <- idR
                      mx <- allT $ folder_depth <+ file_depth
                      return $ 1 + mx
    file_depth = do File {} <- idR 
                    return $ maxi (1::Integer)


-- Histogram

type Histo = Map.Map String Integer

subsystems :: FileObj -> Either String Histo
subsystems = runKureM Right Left . applyT subsystems1 zeroContext

subsystems1 :: TransformE FileObj Histo
subsystems1 = fmap Map.fromList $ allT $ tryM [] go_folder
  where
    go_folder = do Folder s _ _ <- idR
                   i <- countFiles1
                   j <- countFolders1
                   return [(s,i+j)]

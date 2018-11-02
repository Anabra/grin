{-# LANGUAGE LambdaCase #-}
module Transformations.Simplifying.StoreIndexing where

import Data.IntSet (IntSet)
import qualified Data.IntSet as ISet

import Control.Monad
import Control.Monad.Gen
import Control.Monad.State
import Data.Functor.Foldable as Foldable

import Grin.Grin
import Transformations.Util


-- | This transformation indexes the unindexed store operations in the syntax tree.
-- The first new index will be one higher than the current maximal index present in the AST.
storeIndexing :: Exp -> Exp 
storeIndexing e = runGenFrom maxHeapLoc . cataM alg $ e where 
  alg :: ExpF Exp -> Gen Int Exp
  alg = \case 
    SStoreIF Nothing val -> do 
      ind <- gen 
      pure $ SStoreI (Just ind) val 
    e -> pure . embed $ e

  maxHeapLoc :: Int
  maxHeapLoc = if ISet.null locs then (-1) else ISet.findMax locs
    where locs = usedHeapLocs e

usedHeapLocs :: Exp -> IntSet
usedHeapLocs = flip execState mempty . cataM alg where 
  alg :: ExpF () -> State IntSet () 
  alg = \case 
    SStoreIF (Just ind) val -> modify (ISet.insert ind) 
    e -> pure ()


-- | This transformation reindexes all store operations in the syntax tree.
-- Please note that by reindexing already indexed stores, this transformation 
-- will invalidate the #undefined values' types (whenever they contain a location).
storeReIndexing :: Exp -> Exp 
storeReIndexing = runGenFrom (-1) . cataM alg where
  alg :: ExpF Exp -> Gen Int Exp
  alg = \case 
    SStoreIF _ val -> do 
      ind <- gen 
      pure $ SStoreI (Just ind) val 
    e -> pure . embed $ e
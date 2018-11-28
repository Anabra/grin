{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Transformations.Optimising.NonSharedElimination where

{-
Remove the updates that update only non-shared locations.
-}

import Control.Monad.Trans.Except

import Data.Functor.Foldable as Foldable
import Lens.Micro
import Data.Maybe
import qualified Data.Set as Set

import Grin.Grin
import Grin.TypeEnv
import Pipeline.Utils
import Pipeline.Definitions
import AbstractInterpretation.SharingResult

nonSharedEliminationM :: ExceptT String PipelineM Exp
nonSharedEliminationM = do
  exp      <- getExp
  typeEnv  <- getTypeEnv
  shResult <- getSharingResult
  pure $ nonSharedElimination shResult typeEnv exp

nonSharedElimination :: SharingResult -> TypeEnv -> Exp -> Exp
nonSharedElimination SharingResult{..} te = cata skipUpdate where

  -- Remove bind when the parameter points to non-shared locations only.
  skipUpdate :: ExpF Exp -> Exp
  skipUpdate = \case
    EBindF (SUpdate p _) _ rhs
      | all notShared . ptrLocations te $ p -> rhs
    exp -> embed exp

  notShared :: Loc -> Bool
  notShared l = not $ Set.member l _sharedLocs

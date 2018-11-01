{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveFunctor, TypeFamilies #-}
{-# LANGUAGE DeriveFoldable, DeriveTraversable, PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell, StandaloneDeriving #-}

module Grin.Syntax 
  ( module Grin.Syntax 
  , module Grin.SyntaxDefs
  ) where

import Data.Functor.Foldable.TH
import Control.DeepSeq
import GHC.Generics (Generic)
import Data.Int
import Data.Word
import Data.List (isPrefixOf)
import qualified Data.ByteString.Short as B

import Grin.SyntaxDefs
import Grin.TypeEnvDefs

data Name2
  = Name        B.ShortByteString
  | DerivedName B.ShortByteString Int
  | NewName     Name2 Int -- Block scope with shadowing support
  deriving (Ord, Eq, Show)

isPrimName :: Name -> Bool
isPrimName = isPrefixOf "_prim_"

-- * GRIN Literal

-- QUESTION: Now #undefined can be pattern matched on.
-- Should the linter warn about this?
data Lit
  = LInt64  Int64
  | LWord64 Word64
  | LFloat  Float
  | LBool   Bool
  deriving (Generic, NFData, Eq, Ord, Show)

-- * GRIN Value

type LPat = Val -- ConstTagNode, VarTagNode, ValTag, Unit, Lit, Var
type SimpleVal = Val

data Val
  = ConstTagNode  Tag  [{-Simple-}Val] -- complete node (constant tag) ; HIGH level GRIN
  | VarTagNode    Name [{-Simple-}Val] -- complete node (variable tag)
  | ValTag        Tag
  | Unit                           -- HIGH level GRIN
  -- simple val
  | Lit Lit                        -- HIGH level GRIN
  | Var Name                       -- HIGH level GRIN
  | Undefined     Type
  deriving (Generic, NFData, Eq, Ord, Show)

-- See: https://github.com/ekmett/recursion-schemes/blob/master/Data/Functor/Foldable/TH.hs#L31
makeBaseFunctor ''Val

-- * Case Pattern

data CPat
  = NodePat Tag [Name]  -- HIGH level GRIN
  | LitPat  Lit         -- HIGH level GRIN
  | DefaultPat          -- HIGH level GRIN
  | TagPat  Tag
  deriving (Generic, NFData, Eq, Show, Ord)

-- * GRIN Expression

type SimpleExp = Exp
type Alt = Exp
type Def = Exp
type Program = Exp

data Exp
  = Program     [{-Def-}Exp]
  | Def         Name [Name] Exp
  -- Exp
  | EBind       {-Simple-}Exp LPat Exp
  | ECase       Val [{-Alt-}Exp]
  -- Simple Exp
  | SApp        Name [SimpleVal]
  | SReturn     Val
  | SStoreI     (Maybe Int) Val -- give a unique index to the store operation
  | SFetchI     Name (Maybe Int) -- fetch a full node or a single node item in low level GRIN
  | SUpdate     Name Val
  | SBlock      Exp
  -- Alt
  | Alt CPat Exp
  deriving (Generic, NFData, Eq, Ord, Show)

-- See: https://github.com/ekmett/recursion-schemes/blob/master/Data/Functor/Foldable/TH.hs#L31
makeBaseFunctor ''Exp

deriving instance Show a  => Show (ExpF a)
deriving instance Eq a    => Eq   (ExpF a)
deriving instance Ord a   => Ord  (ExpF a)

pattern SFetch  name = SFetchI  name Nothing
pattern SFetchF name = SFetchIF name Nothing

pattern SStore  name = SStoreI  Nothing name
pattern SStoreF name = SStoreIF Nothing name
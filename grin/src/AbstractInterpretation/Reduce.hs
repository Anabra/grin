{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell, ViewPatterns #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module AbstractInterpretation.Reduce where

import Control.DeepSeq
import Data.Int
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Bimap as Bimap
import qualified Data.Foldable
import Data.Function (on)
import GHC.Generics (Generic)

import Control.Monad.State.Strict
import Lens.Micro.Platform

import AbstractInterpretation.IR
import AbstractInterpretation.Util

newtype NodeSet = NodeSet {_nodeTagMap :: Map Tag (Vector (Set Int32))}
  deriving (Eq, Show, Generic, NFData)

data Value
  = Value
  { _simpleType :: Set Int32
  , _nodeSet    :: NodeSet
  }
  deriving (Eq, Show, Generic, NFData)

data ComputerState
  = ComputerState
  { _memory    :: Vector NodeSet
  , _register  :: Vector Value
  }
  deriving (Eq, Show, Generic, NFData)

data AbstractInterpretationResult
  = AbsIntResult
  { _airComp :: ComputerState
  , _airIter :: !Int
  }
  deriving (Eq, Show, Generic, NFData)

concat <$> mapM makeLenses [''NodeSet, ''Value, ''ComputerState, ''AbstractInterpretationResult]

type AbstractComputation = State ComputerState

instance Semigroup NodeSet where (<>)   = unionNodeSet
instance Monoid    NodeSet where mempty = NodeSet mempty

instance Semigroup Value where (<>)   = unionValue
instance Monoid    Value where mempty = Value mempty mempty

unionNodeSet :: NodeSet -> NodeSet -> NodeSet
unionNodeSet (NodeSet x) (NodeSet y) = NodeSet $ Map.unionWith unionNodeData x y where
  unionNodeData a b
    | V.length a == V.length b = V.zipWith Set.union a b
    | otherwise = error $ "node arity mismatch " ++ show (V.length a) ++ " =/= " ++ show (V.length b)

unionValue :: Value -> Value -> Value
unionValue a b = Value
  { _simpleType = Set.union (_simpleType a) (_simpleType b)
  , _nodeSet    = unionNodeSet (_nodeSet a) (_nodeSet b)
  }

regIndex :: Reg -> Int
regIndex (Reg i) = fromIntegral i

memIndex :: Mem -> Int
memIndex (Mem i) = fromIntegral i

selectLoc l = memory.ix (fromIntegral l)

selectReg r = register.ix (regIndex r)

selectSimpleType r = selectReg r.simpleType

selectTagMap r = selectReg r.nodeSet.nodeTagMap

move :: Reg -> Reg -> AbstractComputation ()
move srcReg dstReg = do
  value <- use $ selectReg srcReg
  selectReg dstReg %= (mappend value)

inRange :: Int32 -> Range -> Bool
inRange n (Range from to) = from <= n && n < to

notInRange :: Int32 -> Range -> Bool
notInRange n = not . inRange n

conditionalMoveValue :: Value -> Predicate -> Value -> Value
conditionalMoveValue (Value st1 ns1) predicate (Value st2 ns2) =
  let movedSt = conditionalMoveSimpleType st1 predicate st2
      movedNs = conditionalMoveNodeSet ns1 predicate ns2
  in Value movedSt movedNs

conditionalMoveSimpleType :: Set Int32 -> Predicate -> Set Int32 -> Set Int32
conditionalMoveSimpleType srcSet predicate dstSet =
  case predicate of
    ValueIn rng ->
      let filteredSrcSet = Set.filter (`inRange` rng) srcSet
      in  mappend filteredSrcSet dstSet
    ValueNotIn rng ->
      let filteredSrcSet = Set.filter (`notInRange` rng) srcSet
      in  mappend filteredSrcSet dstSet
    _ -> mappend srcSet dstSet

conditionalMoveNodeSet :: NodeSet -> Predicate -> NodeSet -> NodeSet
conditionalMoveNodeSet (NodeSet srcTagMap) predicate dstNS@(NodeSet dstTagMap) =
  case predicate of
    TagIn tagSet ->
      let restrictedSrcNS = NodeSet $ Map.restrictKeys srcTagMap tagSet
      in  mappend restrictedSrcNS dstNS
    TagNotIn tagSet ->
      let restrictedSrcNS = NodeSet $ Map.withoutKeys srcTagMap tagSet
      in  mappend restrictedSrcNS dstNS
    ValueIn rng ->
      let filteredSrcNS = NodeSet $ Map.map (V.map (Set.filter (`inRange` rng))) srcTagMap
      in mappend filteredSrcNS dstNS
    ValueNotIn rng ->
      let filteredSrcNS = NodeSet $ Map.map (V.map (Set.filter (`notInRange` rng))) srcTagMap
      in mappend filteredSrcNS dstNS

-- NOTE: a ~ Tag
tagPredicateCondition :: ((a          -> Bool) -> Set a -> Bool)
                      -> (a -> Set a -> Bool)
                      -> Map a b
                      -> Bool
tagPredicateCondition quantifier predicate tagMap =
  let tags = Map.keysSet tagMap
  in quantifier (`predicate` tags) tags

-- Note the existential quantifier for the simpleTypes.
-- A field only satisfies the predicate
-- if it actually has a value inside it that satisfies the predicate.
valPredicateCondition :: ((Set Int32 -> Bool) -> [Set Int32] -> Bool)
                      -> (Int32 -> Bool)
                      -> Value
                      -> Bool
valPredicateCondition quantifier predicate (Value st ns) =
  let nsVals = concatMap V.toList . Map.elems . _nodeTagMap $ ns
      vals   = st : nsVals
  in quantifier (any predicate) vals

mappendIf :: Monoid m => (m -> Bool) -> m -> m -> m
mappendIf predicate lhs rhs
  | predicate rhs = mappend lhs rhs
  | otherwise     = rhs

evalInstruction :: Instruction -> AbstractComputation ()
evalInstruction = \case
  If {..} -> do
    satisfy <- case condition of
      NodeTypeExists tag -> do
        tagMap <- use $ selectTagMap srcReg
        pure $ Map.member tag tagMap
      SimpleTypeExists ty -> do
        typeSet <- use $ selectReg srcReg.simpleType
        pure $ Set.member ty typeSet
      NotIn tags -> do
        tagMap <- use $ selectTagMap srcReg
        typeSet <- use $ selectReg srcReg.simpleType
        pure $ not (Set.null typeSet) || Data.Foldable.any (`Set.notMember` tags) (Map.keysSet tagMap)
      All (TagIn tagSet) -> do
        tagMap <- use $ selectTagMap srcReg
        pure $ tagPredicateCondition all Set.member tagMap
      All (TagNotIn tagSet) -> do
        tagMap <- use $ selectTagMap srcReg
        pure $ tagPredicateCondition all Set.notMember tagMap
      Any (TagIn tagSet) -> do
        tagMap <- use $ selectTagMap srcReg
        pure $ tagPredicateCondition any Set.member tagMap
      Any (TagNotIn tagSet) -> do
        tagMap <- use $ selectTagMap srcReg
        pure $ tagPredicateCondition any Set.notMember tagMap
      All (ValueIn rng) -> do
        val <- use $ selectReg srcReg
        pure $ valPredicateCondition all (`inRange` rng) val
      All (ValueNotIn rng) -> do
        val <- use $ selectReg srcReg
        pure $ valPredicateCondition all (`notInRange` rng) val
      Any (ValueIn rng) -> do
        val <- use $ selectReg srcReg
        pure $ valPredicateCondition any (`inRange` rng) val
      Any (ValueNotIn rng) -> do
        val <- use $ selectReg srcReg
        pure $ valPredicateCondition any (`notInRange` rng) val
    when satisfy $ mapM_ evalInstruction instructions

  Project {..} -> case srcSelector of
    NodeItem tag itemIndex -> do
      value <- use $ selectTagMap srcReg.at tag.non mempty.ix itemIndex
      selectReg dstReg.simpleType %= (mappend value)

    ConditionAsSelector cond -> case cond of
      NodeTypeExists tag -> do
        tagMap <- use $ selectTagMap srcReg
        case Map.lookup tag tagMap of
          Nothing -> pure ()
          Just v  -> selectReg dstReg.nodeSet %= (mappend $ NodeSet $ Map.singleton tag v)

      SimpleTypeExists ty -> do
        typeSet <- use $ selectReg srcReg.simpleType
        when (Set.member ty typeSet) $ do
          selectReg dstReg.simpleType %= (Set.insert ty)

      NotIn tags -> do
        value <- use $ selectReg srcReg
        tagMap <- use $ selectTagMap srcReg
        typeSet <- use $ selectReg srcReg.simpleType
        let filteredTagMap = Data.Foldable.foldr Map.delete tagMap tags
        when (not (Set.null typeSet) || not (Map.null filteredTagMap)) $ do
          selectReg dstReg.nodeSet %= (mappend $ NodeSet filteredTagMap)
          selectReg dstReg.simpleType %= (mappend typeSet)

    AllFields -> do
      tagMap <- use $ selectTagMap srcReg
      -- the union of the value sets of all fields
      let mergedFields = mconcat . (map Data.Foldable.fold) . Map.elems $ tagMap
      selectReg dstReg.simpleType %= (mappend mergedFields)

  Extend {..} -> do
    -- TODO: support all selectors
    value <- use $ selectReg srcReg.simpleType
    case dstSelector of
      NodeItem tag itemIndex -> selectTagMap dstReg.at tag.non mempty.ix itemIndex %= (mappend value)
      AllFields -> selectTagMap dstReg %= (Map.map (V.map (mappend value)))
      ConditionAsSelector cond -> case cond of
        -- selects all fields/simpleType having at least one possible value satisfying the predicate
        All (ValueIn    rng) -> do
          selectReg dstReg.simpleType %= (mappendIf (any (`inRange` rng)) value)
          selectTagMap dstReg %= Map.map (V.map (mappendIf (any (`inRange` rng)) value))
        All (ValueNotIn rng) -> do
          selectReg dstReg.simpleType %= (mappendIf (any (`notInRange` rng)) value)
          selectTagMap dstReg %= Map.map (V.map (mappendIf (any (`notInRange` rng)) value))

  Move {..} -> move srcReg dstReg

  RestrictedMove {..} -> do
    srcTypeSet <- use $ selectSimpleType srcReg
    selectReg dstReg.simpleType %= (mappend srcTypeSet)

    srcTagMap <- use $ selectTagMap srcReg
    dstTagMap <- use $ selectTagMap dstReg

    let restrictedSrcNodeSet = NodeSet $ Map.intersection srcTagMap dstTagMap
    selectReg dstReg.nodeSet %= (mappend restrictedSrcNodeSet)

  ConditionalMove {..} -> do
    srcVal <- use $ selectReg srcReg
    selectReg dstReg %= (conditionalMoveValue srcVal predicate)

  Fetch {..} -> do
    addressSet <- use $ register.ix (regIndex addressReg).simpleType
    forM_ addressSet $ \address -> when (address >= 0) $ do
      value <- use $ memory.ix (fromIntegral address)
      selectReg dstReg.nodeSet %= (mappend value)

  Store {..} -> do
    value <- use $ selectReg srcReg.nodeSet
    memory.ix (memIndex address) %= (mappend value)

  Update {..} -> do
    value <- use $ selectReg srcReg.nodeSet
    addressSet <- use $ register.ix (regIndex addressReg).simpleType
    forM_ addressSet $ \address -> when (address >= 0) $ do
      memory.ix (fromIntegral address) %= (mappend value)

  RestrictedUpdate {..} -> do
    srcTagMap  <- use $ selectTagMap srcReg
    addressSet <- use $ register.ix (regIndex addressReg).simpleType
    forM_ addressSet $ \address -> when (address >= 0) $ do
      locTagMap <- use $ selectLoc address.nodeTagMap
      let restrictedSrcNodeSet = NodeSet $ Map.intersection srcTagMap locTagMap
      selectLoc address %= (mappend restrictedSrcNodeSet)

  ConditionalUpdate {..} -> do
    srcVal <- use $ selectReg srcReg.nodeSet
    addressSet <- use $ register.ix (regIndex addressReg).simpleType
    forM_ addressSet $ \address -> when (address >= 0) $
      selectLoc address %= (conditionalMoveNodeSet srcVal predicate)

  Set {..} -> case constant of
    CSimpleType ty        -> selectReg dstReg.simpleType %= (mappend $ Set.singleton ty)
    CHeapLocation (Mem l) -> selectReg dstReg.simpleType %= (mappend $ Set.singleton $ fromIntegral l)
    CNodeType tag arity   -> selectReg dstReg.nodeSet %= (mappend $ NodeSet . Map.singleton tag $ V.replicate arity mempty)
    CNodeItem tag idx val -> selectReg dstReg.nodeSet.nodeTagMap.at tag.non mempty.ix idx %= (mappend $ Set.singleton val)

continueAbstractProgramWith :: ComputerState -> AbstractProgram -> AbstractInterpretationResult
continueAbstractProgramWith comp AbstractProgram{..} = converge ((==) `on` (_airComp . force)) step (AbsIntResult comp 0) where
  nextComputer c = execState (mapM_ evalInstruction _absInstructions) c
  step AbsIntResult{..} = AbsIntResult (nextComputer _airComp) (succ _airIter)

evalAbstractProgram :: AbstractProgram -> AbstractInterpretationResult
evalAbstractProgram p@AbstractProgram{..} = continueAbstractProgramWith emptyComputer p where
  emptyComputer = ComputerState
    { _memory   = V.replicate (fromIntegral _absMemoryCounter) mempty
    , _register = V.replicate (fromIntegral _absRegisterCounter) mempty
    }

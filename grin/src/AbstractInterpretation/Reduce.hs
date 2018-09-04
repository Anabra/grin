{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell, ViewPatterns #-}
module AbstractInterpretation.Reduce where

import Data.Int
import Data.Word
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Bimap as Bimap
import qualified Data.Foldable
import Data.Maybe (fromJust)

import Control.Monad.State.Strict
import Lens.Micro.Platform

import Transformations.Util (TagInfo(..))

import AbstractInterpretation.IR as IR
import AbstractInterpretation.CodeGen

import qualified Grin.Grin as Grin (Tag)

newtype NodeSet = NodeSet {_nodeTagMap :: Map Tag (Vector (Set Int32))} deriving (Eq, Show)

data Value
  = Value
  { _simpleType :: Set Int32
  , _nodeSet    :: NodeSet
  }
  deriving (Eq, Show)

data Computer
  = Computer
  { _memory    :: Vector NodeSet
  , _register  :: Vector Value
  -- NOTE: Same as in the AbstractProgram, just with IR tags.
  -- This can probably replaced by a vector,
  -- since IR.Tags are basically positive integers.
  , _tagInfo   :: Map IR.Tag Int
  }
  deriving (Eq, Show)

concat <$> mapM makeLenses [''NodeSet, ''Value, ''Computer]

type AbstractComputation = State Computer

instance Monoid NodeSet where
  mempty  = NodeSet mempty
  mappend = unionNodeSet

instance Monoid Value where
  mempty  = Value mempty mempty
  mappend = unionValue

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

mkNodeWithField :: Int -> Int -> Set Int32 -> Vector (Set Int32)
mkNodeWithField arity idx val = V.fromList $
  empties idx ++ [val] ++ empties (arity-idx-1)
  where empties n = replicate n Set.empty

move :: Reg -> Reg -> AbstractComputation ()
move srcReg dstReg = do
  value <- use $ selectReg srcReg
  selectReg dstReg %= (mappend value)

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
      NotEmpty -> do
        typeSet <- use $ selectReg srcReg.simpleType
        tagMap  <- use $ selectTagMap srcReg
        let hasAnyInfo = any (any (not . Set.null))
        pure $ not (Set.null typeSet) || hasAnyInfo tagMap
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

      NotEmpty -> move srcReg dstReg

  -- NOTE: Now if the tag is not present in dstReg,
  --  it creates a new empty node with the given tag
  --  (using tag information collected during the analysis)
  -- and then extends its given field.
  Extend {..} -> do
    -- TODO: support all selectors
    value <- use $ selectReg srcReg.simpleType
    case dstSelector of
      NodeItem tag itemIndex -> do
        dstTagMap <- use $ selectTagMap dstReg
        case Map.lookup tag dstTagMap of
          Just _  -> selectTagMap dstReg.at tag.non mempty.ix itemIndex %= (mappend value)
          Nothing -> do
            arityM <- use $ tagInfo.at tag
            case arityM of
              Just arity -> do
                let arity = fromJust arityM
                    vec   = mkNodeWithField arity itemIndex value
                selectTagMap dstReg %= (Map.insert tag vec)
              Nothing -> pure ()

        selectTagMap dstReg.at tag.non mempty.ix itemIndex %= (mappend value)
      AllFields -> selectTagMap dstReg %= (Map.map (V.map (mappend value)))

  Move {..} -> move srcReg dstReg

  RestrictedMove {..} -> do
    srcTypeSet <- use $ selectSimpleType srcReg
    selectReg dstReg.simpleType %= (mappend srcTypeSet)

    srcTagMap <- use $ selectTagMap srcReg
    dstTagMap <- use $ selectTagMap dstReg

    let restrictedSrcTagMap = Map.intersection srcTagMap dstTagMap
    selectTagMap dstReg %= (Map.union restrictedSrcTagMap)

  CopyStructure {..} -> do
    srcTagMap <- use $ selectTagMap srcReg
    let emptiedTagMap = Map.map (V.map (const Set.empty)) srcTagMap
    selectTagMap dstReg %= (flip Map.union emptiedTagMap)

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
      let restrictedSrcTagMap = Map.intersection srcTagMap locTagMap
      selectLoc address.nodeTagMap %= (Map.union restrictedSrcTagMap)

  Set {..} -> case constant of
    CSimpleType ty        -> selectReg dstReg.simpleType %= (mappend $ Set.singleton ty)
    CHeapLocation (Mem l) -> selectReg dstReg.simpleType %= (mappend $ Set.singleton $ fromIntegral l)
    CNodeType tag arity   -> selectReg dstReg.nodeSet %=
                                (mappend $ NodeSet . Map.singleton tag $ V.replicate arity mempty)
    CNodeItem tag idx val -> selectReg dstReg.nodeSet.
                                nodeTagMap.at tag.non mempty.ix idx %= (mappend $ Set.singleton val)

evalDataFlowInfoWith :: HasDataFlowInfo s => Computer -> s -> Computer
evalDataFlowInfoWith comp (getDataFlowInfo -> AbstractProgram{..}) = run comp where
  run computer = if computer == nextComputer then computer else run nextComputer
    where nextComputer = execState (mapM_ evalInstruction absInstructions) computer

evalDataFlowInfo :: HasDataFlowInfo s => s -> Computer
evalDataFlowInfo dfi@(getDataFlowInfo -> AbstractProgram{..}) =
  evalDataFlowInfoWith emptyComputer dfi
  where
    emptyComputer = Computer
      { _memory   = V.replicate (fromIntegral absMemoryCounter) mempty
      , _register = V.replicate (fromIntegral absRegisterCounter) mempty
      -- NOTE: converting Grin tags to their IR equivalents
      , _tagInfo  = Map.mapKeys toIR $ tagInfo
      }

    toIR :: Grin.Tag -> IR.Tag
    toIR = (Bimap.!) absTagMap

    tagInfo :: Map Grin.Tag Int
    tagInfo = _tagArityMap absTagInfo

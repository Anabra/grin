{-# LANGUAGE LambdaCase, RecordWildCards, TupleSections, RankNTypes, ViewPatterns #-}
module AbstractInterpretation.CodeGen where

import Control.Monad.Trans.Except
import Data.Word
import qualified Data.Bimap as Bimap
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State

import Grin.Grin
import Transformations.Util (TagInfo(..), updateTagInfo)
import qualified AbstractInterpretation.IR as IR
import AbstractInterpretation.IR (AbstractProgram(..), HasDataFlowInfo(..))

type CG s a = ExceptT String (State s) a

data Result s
  = R IR.Reg
  | Z
  | A CPat (CG s (Result s))

emit :: HasDataFlowInfo s => IR.Instruction -> CG s ()
emit inst = modify' $ modifyInfo $ \s@AbstractProgram{..} -> s {absInstructions = inst : absInstructions}

getsDfi :: (HasDataFlowInfo s, MonadState s m) => (AbstractProgram -> a) -> m a
getsDfi f = gets (f . getDataFlowInfo)

stateDfi :: (HasDataFlowInfo s, MonadState s m) =>
            (AbstractProgram -> (a, AbstractProgram)) -> m a
stateDfi f = do
  (res, dfi) <- getsDfi f
  modify $ modifyInfo $ const dfi
  return res

-- creates regsiters for function arguments and result
getOrAddFunRegs :: HasDataFlowInfo s => Name -> Int -> CG s (IR.Reg, [IR.Reg])
getOrAddFunRegs name arity = do
  funMap <- getsDfi absFunctionArgMap
  case Map.lookup name funMap of
    Just x  -> pure x
    Nothing -> do
      resReg <- newReg
      argRegs <- replicateM arity newReg
      let funRegs = (resReg, argRegs)
      modify' $ modifyInfo $ \s@AbstractProgram{..} -> s {absFunctionArgMap = Map.insert name funRegs absFunctionArgMap}
      pure funRegs

newReg :: HasDataFlowInfo s => CG s IR.Reg
newReg = stateDfi $ \s@AbstractProgram{..} -> (IR.Reg absRegisterCounter, s {absRegisterCounter = succ absRegisterCounter})

newMem :: HasDataFlowInfo s => CG s IR.Mem
newMem = stateDfi $ \s@AbstractProgram{..} -> (IR.Mem absMemoryCounter, s {absMemoryCounter = succ absMemoryCounter})

addReg :: HasDataFlowInfo s => Name -> IR.Reg -> CG s ()
addReg name reg = modify' $ modifyInfo $ \s@AbstractProgram{..} -> s {absRegisterMap = Map.insert name reg absRegisterMap}

getReg :: HasDataFlowInfo s => Name -> CG s IR.Reg
getReg name = do
  regMap <- getsDfi absRegisterMap
  case Map.lookup name regMap of
    Nothing   -> throwE $ "unknown variable " ++ name
    Just reg  -> pure reg

getTag :: HasDataFlowInfo s => Tag -> CG s IR.Tag
getTag tag = do
  tagMap <- getsDfi absTagMap
  case Bimap.lookup tag tagMap of
    Just t  -> pure t
    Nothing -> do
      let t = IR.Tag . fromIntegral $ Bimap.size tagMap
      modify' $ modifyInfo $ \s -> s {absTagMap = Bimap.insert tag t tagMap}
      pure t

addTagInfo :: HasDataFlowInfo s => Tag -> Int -> CG s ()
addTagInfo tag arity = modify' . modifyInfo
  $ \s@AbstractProgram{..} -> s { absTagInfo = updateTagInfo tag arity absTagInfo }


codeGenBlock :: HasDataFlowInfo s => CG s () -> CG s [IR.Instruction]
codeGenBlock genM = do
  instructions <- stateDfi $ \s@AbstractProgram{..} -> (absInstructions, s {absInstructions = []})
  genM
  blockInstructions <- stateDfi $ \s@AbstractProgram{..} -> (reverse absInstructions, s {absInstructions = instructions})
  pure blockInstructions

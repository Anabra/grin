{-# LANGUAGE OverloadedStrings #-}
module Reducer.LLVM.PrimOps where

import Control.Monad (when)
import LLVM.AST
import qualified LLVM.AST.IntegerPredicate as I
import qualified LLVM.AST.FloatingPointPredicate as F
import qualified LLVM.AST.CallingConvention as CC
import LLVM.AST.Type as LLVM
import LLVM.AST.AddrSpace
import qualified LLVM.AST.Constant as C

import qualified Grin.Grin as Grin
import Grin.TypeEnv hiding (function)
import Reducer.LLVM.Base
import Reducer.LLVM.TypeGen
import Grin.PrimOpsPrelude


cgUnit    = toCGType $ T_SimpleType T_Unit    :: CGType
cgInt64   = toCGType $ T_SimpleType T_Int64   :: CGType
cgWord64  = toCGType $ T_SimpleType T_Word64  :: CGType
cgFloat   = toCGType $ T_SimpleType T_Float   :: CGType
cgBool    = toCGType $ T_SimpleType T_Bool    :: CGType
cgString  = toCGType $ T_SimpleType T_String  :: CGType
cgChar    = toCGType $ T_SimpleType T_Char    :: CGType

codeExternal :: Grin.External -> [Operand] -> CG (Either String Result)
codeExternal e ops = case Grin.eKind e of
  Grin.PrimOp -> codeGenPrimOp (Grin.eName e) ops
  Grin.FFI    -> codeGenFFI     e             ops

codeGenPrimOp :: Grin.Name -> [Operand] -> CG (Either String Result)
codeGenPrimOp name [opA, opB] = pure $ case name of
  -- Int
  "_prim_int_add"   -> Right . I cgInt64 $ Add  {nsw=False, nuw=False, operand0=opA, operand1=opB, metadata=[]}
  "_prim_int_sub"   -> Right . I cgInt64 $ Sub  {nsw=False, nuw=False, operand0=opA, operand1=opB, metadata=[]}
  "_prim_int_mul"   -> Right . I cgInt64 $ Mul  {nsw=False, nuw=False, operand0=opA, operand1=opB, metadata=[]}
  "_prim_int_div"   -> Right . I cgInt64 $ SDiv {exact=False, operand0=opA, operand1=opB, metadata=[]}
  "_prim_int_eq"    -> Right . I cgBool  $ ICmp {iPredicate=I.EQ,  operand0=opA, operand1=opB, metadata=[]}
  "_prim_int_ne"    -> Right . I cgBool  $ ICmp {iPredicate=I.NE,  operand0=opA, operand1=opB, metadata=[]}
  "_prim_int_gt"    -> Right . I cgBool  $ ICmp {iPredicate=I.SGT, operand0=opA, operand1=opB, metadata=[]}
  "_prim_int_ge"    -> Right . I cgBool  $ ICmp {iPredicate=I.SGE, operand0=opA, operand1=opB, metadata=[]}
  "_prim_int_lt"    -> Right . I cgBool  $ ICmp {iPredicate=I.SLT, operand0=opA, operand1=opB, metadata=[]}
  "_prim_int_le"    -> Right . I cgBool  $ ICmp {iPredicate=I.SLE, operand0=opA, operand1=opB, metadata=[]}

  -- Word
  "_prim_word_add"  -> Right . I cgWord64 $ Add  {nsw=False, nuw=False, operand0=opA, operand1=opB, metadata=[]}
  "_prim_word_sub"  -> Right . I cgWord64 $ Sub  {nsw=False, nuw=False, operand0=opA, operand1=opB, metadata=[]}
  "_prim_word_mul"  -> Right . I cgWord64 $ Mul  {nsw=False, nuw=False, operand0=opA, operand1=opB, metadata=[]}
  "_prim_word_div"  -> Right . I cgWord64 $ UDiv {exact=False, operand0=opA, operand1=opB, metadata=[]}
  "_prim_word_eq"   -> Right . I cgBool   $ ICmp {iPredicate=I.EQ,  operand0=opA, operand1=opB, metadata=[]}
  "_prim_word_ne"   -> Right . I cgBool   $ ICmp {iPredicate=I.NE,  operand0=opA, operand1=opB, metadata=[]}
  "_prim_word_gt"   -> Right . I cgBool   $ ICmp {iPredicate=I.UGT, operand0=opA, operand1=opB, metadata=[]}
  "_prim_word_ge"   -> Right . I cgBool   $ ICmp {iPredicate=I.UGE, operand0=opA, operand1=opB, metadata=[]}
  "_prim_word_lt"   -> Right . I cgBool   $ ICmp {iPredicate=I.ULT, operand0=opA, operand1=opB, metadata=[]}
  "_prim_word_le"   -> Right . I cgBool   $ ICmp {iPredicate=I.ULE, operand0=opA, operand1=opB, metadata=[]}

  -- Float
  "_prim_float_add" -> Right . I cgFloat $ FAdd {fastMathFlags=noFastMathFlags, operand0=opA, operand1=opB, metadata=[]}
  "_prim_float_sub" -> Right . I cgFloat $ FSub {fastMathFlags=noFastMathFlags, operand0=opA, operand1=opB, metadata=[]}
  "_prim_float_mul" -> Right . I cgFloat $ FMul {fastMathFlags=noFastMathFlags, operand0=opA, operand1=opB, metadata=[]}
  "_prim_float_div" -> Right . I cgFloat $ FDiv {fastMathFlags=noFastMathFlags, operand0=opA, operand1=opB, metadata=[]}
  "_prim_float_eq"  -> Right . I cgBool  $ FCmp {fpPredicate=F.OEQ, operand0=opA, operand1=opB, metadata=[]}
  "_prim_float_ne"  -> Right . I cgBool  $ FCmp {fpPredicate=F.ONE, operand0=opA, operand1=opB, metadata=[]}
  "_prim_float_gt"  -> Right . I cgBool  $ FCmp {fpPredicate=F.OGT, operand0=opA, operand1=opB, metadata=[]}
  "_prim_float_ge"  -> Right . I cgBool  $ FCmp {fpPredicate=F.OGE, operand0=opA, operand1=opB, metadata=[]}
  "_prim_float_lt"  -> Right . I cgBool  $ FCmp {fpPredicate=F.OLT, operand0=opA, operand1=opB, metadata=[]}
  "_prim_float_le"  -> Right . I cgBool  $ FCmp {fpPredicate=F.OLE, operand0=opA, operand1=opB, metadata=[]}

  -- Bool
  "_prim_bool_eq"   -> Right . I cgBool $ ICmp {iPredicate=I.EQ,  operand0=opA, operand1=opB, metadata=[]}
  "_prim_bool_ne"   -> Right . I cgBool $ ICmp {iPredicate=I.NE,  operand0=opA, operand1=opB, metadata=[]}

  _ -> Left $ "unknown primop: " ++ show name

codeGenFFI :: Grin.External -> [Operand] -> CG (Either String Result)
codeGenFFI e ops = do
  if (length ops /= length (Grin.eArgsType e))
    then pure $ Left $ "Non saturated function call: " ++ show (e, ops)
    else Right <$> mkFunction (Grin.nameString $ Grin.eName e) (ops `zip` (Grin.eArgsType e)) (Grin.eRetType e)

mkFunction name ops_params_ty ret_ty = pure . I (tyToCGType ret_ty) $ Call
    { tailCallKind = Nothing
    , callingConvention = CC.C
    , returnAttributes = []
    , function = Right $ ConstantOperand $ C.GlobalReference (fun (tyToLLVMType ret_ty) (tyToLLVMType <$> params_ty)) (mkName name)
    , arguments = ops `zip` repeat []
    , functionAttributes = []
    , metadata = []
    }
  where
    (ops, params_ty) = unzip ops_params_ty
    tyToLLVMType t = case t of
        Grin.TySimple st -> typeGenSimpleType st
        _                -> error $ "Non simple type in: " ++ show (name, t)
    tyToCGType t = case t of
        Grin.TySimple st -> toCGType (T_SimpleType st)
        _                -> error $ "Non simple type in: " ++ show (name, t)
    fptr ty = PointerType { pointerReferent = ty, pointerAddrSpace = AddrSpace 0}
    fun ret args = fptr FunctionType {resultType = ret, argumentTypes = args, isVarArg = False}

module LiveVariable.LiveVariableSpec where

import System.FilePath

import Grin.Grin

import Test.IO
import Test.Hspec

import AbstractInterpretation.Reduce (evalDataFlowInfo, evalDataFlowInfoWith)
import AbstractInterpretation.HeapPointsTo as HPT
import AbstractInterpretation.LiveVariable as LVA
import AbstractInterpretation.LVAResult

import LiveVariable.Tests.Util
import LiveVariable.Tests.CaseBackwardPropagation
import LiveVariable.Tests.CaseBackwardPropagationNodes
import LiveVariable.Tests.Fields
import LiveVariable.Tests.FunctionCall1
import LiveVariable.Tests.FunctionCall2
import LiveVariable.Tests.HeapCaseMin
import LiveVariable.Tests.HeapCase
import LiveVariable.Tests.HeapSimple
import LiveVariable.Tests.LitPat

spec :: Spec
spec = runIO runTests

runTests :: IO ()
runTests = runTestsFrom stackRoot

runTestsGHCi :: IO ()
runTestsGHCi = runTestsFrom stackSrc

runTestsFrom :: FilePath -> IO ()
runTestsFrom fromCurDir = runTestsFromWith fromCurDir calcLiveness
  [ caseBackwardPropagationSrc
  , caseBackwardPropagationNodesSrc
  , fieldsSrc
  , functionCall1Src
  , functionCall2Src
  , heapCaseMinSrc
  , heapCaseSrc
  , heapSimpleSrc
  , litPatSrc
  ]
  [ caseBackwardPropagationSpec
  , caseBackwardPropagationNodesSpec
  , fieldsSpec
  , functionCall1Spec
  , functionCall2Spec
  , heapCaseMinSpec
  , heapCaseSpec
  , heapSimpleSpec
  , litPatSpec
  ]

calcLiveness :: Exp -> LVAResult
calcLiveness prog
  | Right hptProgram <- HPT.codeGen prog
  , Right lvaProgram <- LVA.codeGen hptProgram prog
  , hptComputer <- evalDataFlowInfo hptProgram
  , lvaComputer <- evalDataFlowInfoWith hptComputer lvaProgram
  = toLVAResult lvaProgram lvaComputer

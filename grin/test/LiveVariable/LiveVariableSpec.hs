module LiveVariable.LiveVariableSpec where

import System.FilePath

import Grin.Grin

import Test.IO
import Test.Hspec

import AbstractInterpretation.Reduce (evalDataFlowInfo)
import AbstractInterpretation.LiveVariable
import AbstractInterpretation.LVAResult

import LiveVariable.Tests.Util
import LiveVariable.Tests.CaseAnonymous
import LiveVariable.Tests.CaseMinLit
import LiveVariable.Tests.CaseMinNodes
import LiveVariable.Tests.CaseNested
import LiveVariable.Tests.CaseRestricted
import LiveVariable.Tests.CaseRestrictedNodes
import LiveVariable.Tests.Fields
import LiveVariable.Tests.FunctionCall1
import LiveVariable.Tests.FunctionCall2
import LiveVariable.Tests.HeapCaseMin
import LiveVariable.Tests.HeapCase
import LiveVariable.Tests.HeapIndirectSimple
import LiveVariable.Tests.HeapSimple
import LiveVariable.Tests.HeapUpdateComplex
import LiveVariable.Tests.HeapUpdateFunCall
import LiveVariable.Tests.HeapUpdateLocal
import LiveVariable.Tests.IndexedStore
import LiveVariable.Tests.LitPat
import LiveVariable.Tests.MainNodeRet
import LiveVariable.Tests.NodesSimple
import LiveVariable.Tests.NodesTricky
import LiveVariable.Tests.SumOpt

spec :: Spec
spec = runIO runTests

runTests :: IO ()
runTests = runTestsFrom stackRoot

runTestsGHCi :: IO ()
runTestsGHCi = runTestsFrom stackTest

lvaTestName :: String 
lvaTestName = "Live Variable Analysis"

runTestsFrom :: FilePath -> IO ()
runTestsFrom fromCurDir = testGroup lvaTestName $
  mkSpecFromWith fromCurDir calcLiveness
    [ caseAnonymousSrc
    , caseMinLitSrc
    , caseMinNodesSrc
    , caseNestedSrc
    , caseRestrictedSrc
    , caseRestrictedNodesSrc
    , fieldsSrc
    , functionCall1Src
    , functionCall2Src
    , heapCaseMinSrc
    , heapCaseSrc
    , heapIndirectSimpleSrc
    , heapSimpleSrc
    , heapUpdateComplexSrc
    , heapUpdateFunCallSrc
    , heapUpdateLocalSrc
    , indexedStoreSrc
    , litPatSrc
    , mainNodeRetSrc
    , nodesSimpleSrc
    , nodesTrickySrc
    , sumOptSrc
    ]
    [ caseAnonymousSpec
    , caseMinLitSpec
    , caseMinNodesSpec
    , caseNestedSpec
    , caseRestrictedSpec
    , caseRestrictedNodesSpec
    , fieldsSpec
    , functionCall1Spec
    , functionCall2Spec
    , heapCaseMinSpec
    , heapCaseSpec
    , heapIndirectSimpleSpec
    , heapSimpleSpec
    , heapUpdateComplexSpec
    , heapUpdateFunCallSpec
    , heapUpdateLocalSpec
    , indexedStoreSpec
    , litPatSpec
    , mainNodeRetSpec
    , nodesSimpleSpec
    , nodesTrickySpec
    , sumOptSpec
    ]

calcLiveness :: Exp -> LVAResult
calcLiveness prog
  | Right lvaProgram <- codeGen prog
  , computer <- evalDataFlowInfo lvaProgram
  = toLVAResult lvaProgram computer

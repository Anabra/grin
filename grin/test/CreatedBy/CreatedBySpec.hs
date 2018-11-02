module CreatedBy.CreatedBySpec where

import Data.Monoid ((<>))
import qualified Data.Map as M
import qualified Data.Set as S

import System.FilePath

import Grin.Grin

import Test.IO
import Test.Test
import Test.Util
import Test.Hspec
import Test.Assertions

import AbstractInterpretation.IR hiding (Tag)
import AbstractInterpretation.Reduce
import AbstractInterpretation.CreatedBy
import AbstractInterpretation.CByResult


spec :: Spec
spec = runIO runTests

runTests :: IO ()
runTests = runTestsFrom stackRoot

runTestsGHCi :: IO ()
runTestsGHCi = runTestsFrom stackTest

cbyTestName :: String 
cbyTestName = "Created-By"

runTestsFrom :: FilePath -> IO ()
runTestsFrom fromCurDir = testGroup cbyTestName $
  mkSpecFromWith fromCurDir calcProducers
    [ puresSrc
    , funCallSrc
    , caseSimpleSrc
    , heapSrc
    , pointerInNodeSrc
    , caseRestricted1Src
    , caseRestricted2Src
    , caseRestricted3Src
    , undefinedSrc
    , indexedStoreSrc
    ]
    [ puresSpec
    , funCallSpec
    , caseSimpleSpec
    , heapSpec
    , pointerInNodeSpec
    , caseRestricted1Spec
    , caseRestricted2Spec
    , caseRestricted3Spec
    , undefinedSpec
    , indexedStoreSpec
    ]

cbyExamples :: FilePath
cbyExamples = "CreatedBy" </> "examples"

calcCByResult :: Exp -> CByResult
calcCByResult prog
  | Right cbyProgram <- codeGen prog
  , computer <- evalDataFlowInfo cbyProgram
  , cbyResult <- toCByResult cbyProgram computer
  = cbyResult

calcProducers :: Exp -> ProducerMap
calcProducers = _producers . calcCByResult

mkProducerSet :: [(Tag, [Name])] -> ProducerSet
mkProducerSet = ProducerSet . M.fromList . map (\(t,xs) -> (t,S.fromList xs))

emptyProducerSet :: ProducerSet
emptyProducerSet = mkProducerSet []

restrictedBy :: ProducerSet -> Tag -> ProducerSet
restrictedBy (ProducerSet ps) tag = ProducerSet $ M.filterWithKey (\k _ -> k == tag) ps

udProd :: String 
udProd = undefinedProducerName


puresSrc :: FilePath
puresSrc = cbyExamples </> "pures.grin"

puresExpected :: ProducerMap
puresExpected = ProducerMap $
  M.fromList [ ("a", producerA)
             , ("b", producerA)
             , ("c", producerA)
             ]
  where producerA = mkProducerSet [(cInt, ["a"])]

puresSpec :: ProducerMap -> Spec
puresSpec found = it "pures" $ found `sameAs` puresExpected



funCallSrc :: FilePath
funCallSrc = cbyExamples </> "function_call.grin"

funCallExpected :: ProducerMap
funCallExpected = ProducerMap $
  M.fromList [ ("a",  producerA)
             , ("b",  producerA)
             , ("c",  producerX1)
             , ("d",  producerX1)
             , ("x",  emptyProducerSet)
             , ("x1", producerX1)
             , ("y",  emptyProducerSet)
             , ("y1", producerX1)
             ]
  where producerA  = mkProducerSet [(cInt, ["a"])]
        producerX1 = mkProducerSet [(cInt, ["x1"])]

funCallSpec :: ProducerMap -> Spec
funCallSpec found = it "function_call" $ found `sameAs` funCallExpected



caseSimpleSrc :: FilePath
caseSimpleSrc = cbyExamples </> "case_simple.grin"

caseSimpleExpected :: ProducerMap
caseSimpleExpected = ProducerMap $
  M.fromList [ ("a",  producerA)
             , ("x",  emptyProducerSet)
             , ("x0", producerX0)
             , ("x1", producerX1)
             ]
  where producerA  = mkProducerSet [ (cInt,  ["x0"])
                                   , (cBool, ["x1"])
                                   ]
        producerX0 = mkProducerSet [(cInt,  ["x0"])]
        producerX1 = mkProducerSet [(cBool, ["x1"])]

caseSimpleSpec :: ProducerMap -> Spec
caseSimpleSpec found = it "case_simple" $ found `sameAs` caseSimpleExpected



heapSrc :: FilePath
heapSrc = cbyExamples </> "heap.grin"

heapExpected :: ProducerMap
heapExpected = ProducerMap $
  M.fromList [ ("x0", producerX0)
             , ("x1", producerX1)
             , ("x2", producerX2)
             , ("p0", emptyProducerSet)
             , ("p1", emptyProducerSet)
             , ("y0", producerY0)
             , ("y1", producerY1)
             ]
  where producerX0 = mkProducerSet [(cInt,  ["x0"])]
        producerX1 = mkProducerSet [(cBool, ["x1"])]
        producerX2 = mkProducerSet [(cBool, ["x2"])]
        producerY0 = producerX0 <> producerX2
        producerY1 = producerX1 <> producerX2

heapSpec :: ProducerMap -> Spec
heapSpec found = it "heap" $ found `sameAs` heapExpected



caseRestricted1Src :: FilePath
caseRestricted1Src = cbyExamples </> "case_restricted_1.grin"

caseRestricted1Expected :: ProducerMap
caseRestricted1Expected = ProducerMap $
  M.fromList [ ("a0", producerA0)
             , ("r0", producerR0)
             , ("b0", producerB0)
             , ("b1", producerB1)
             , ("b2", emptyProducerSet)
             , ("c0", emptyProducerSet)
             , ("c1", emptyProducerSet)
             , ("c2", emptyProducerSet)
             , ("x",  emptyProducerSet)
             , ("x0", producerX0)
             , ("x1", producerX1)
             ]
  where producerX0 = mkProducerSet [(cInt,  ["x0"])]
        producerX1 = mkProducerSet [(cBool, ["x1"])]
        producerA0 = producerX0 <> producerX1
        producerB0 = mkProducerSet [(cInt,  ["b0"])]
        producerB1 = mkProducerSet [(cBool, ["b1"])]
        producerR0 = producerB0 <> producerB1

caseRestricted1Spec :: ProducerMap -> Spec
caseRestricted1Spec found = it "case_restricted_1" $ found `sameAs` caseRestricted1Expected



caseRestricted2Src :: FilePath
caseRestricted2Src = cbyExamples </> "case_restricted_2.grin"

caseRestricted2Expected :: ProducerMap
caseRestricted2Expected = ProducerMap $
  M.fromList [ ("a0", producerA0)
             , ("r0", producerR0)
             , ("b0", producerB0)
             , ("b1", producerB1)
             , ("b2", emptyProducerSet)
             , ("c0", emptyProducerSet)
             , ("c1", emptyProducerSet)
             , ("c2", emptyProducerSet)
             , ("x",  emptyProducerSet)
             , ("x0", producerX0)
             , ("x1", producerX1)
             ]
  where producerX0 = mkProducerSet [(cInt,  ["x0"])]
        producerX1 = mkProducerSet [(cBool, ["x1"])]
        producerA0 = producerX0 <> producerX1
        producerB0 = producerX0 <> producerX1
        producerB1 = mkProducerSet [(cBool, ["b1"])]
        producerR0 = producerB0 <> producerB1

caseRestricted2Spec :: ProducerMap -> Spec
caseRestricted2Spec found = it "case_restricted_2" $ found `sameAs` caseRestricted2Expected



caseRestricted3Src :: FilePath
caseRestricted3Src = cbyExamples </> "case_restricted_3.grin"

caseRestricted3Expected :: ProducerMap
caseRestricted3Expected = ProducerMap $
  M.fromList [ ("a0", producerA0)
             , ("a1", producerA1)
             , ("r0", producerR0)
             , ("b0", producerB0)
             , ("b1", producerB1)
             , ("b2", emptyProducerSet)
             , ("c0", emptyProducerSet)
             , ("c1", emptyProducerSet)
             , ("c2", emptyProducerSet)
             , ("x",  emptyProducerSet)
             , ("x0", producerX0)
             , ("x1", producerX1)
             , ("y",  producerY)
             , ("y0", producerY0)
             , ("y1", emptyProducerSet) -- because the control never reaches it
             , ("y2", producerY2)
             , ("n",  emptyProducerSet)
             , ("b",  emptyProducerSet)
             , ("w",  emptyProducerSet)
             ]
  where producerX0 = mkProducerSet [(cInt,  ["x0"])]
        producerX1 = mkProducerSet [(cBool, ["x1"])]
        producerA0 = producerX0 <> producerX1
        producerA1 = mkProducerSet [(cWord, ["a1"])]
        producerY  = producerA0 `restrictedBy` (cInt) <> producerA1
        producerY0 = mkProducerSet [(cInt,  ["y0"])]
        producerY1 = mkProducerSet [(cBool, ["y1"])]
        producerY2 = mkProducerSet [(cWord, ["y2"])]
        producerB0 = producerY0 <> producerY2 -- because the analysis is not context sensitive
        producerB1 = producerY0 <> producerY2 -- because the analysis is not context sensitive
        producerR0 = producerB0 <> producerB1

caseRestricted3Spec :: ProducerMap -> Spec
caseRestricted3Spec found = it "case_restricted_3" $ found `sameAs` caseRestricted3Expected



pointerInNodeSrc :: FilePath
pointerInNodeSrc = cbyExamples </> "pointer_in_node.grin"

pointerInNodeExpected :: ProducerMap
pointerInNodeExpected = ProducerMap $
  M.fromList [ ("n0",  producerN0)
             , ("p0",  emptyProducerSet)
             , ("n1",  producerN1)
             , ("x",   emptyProducerSet)
             , ("pxs", emptyProducerSet)
             , ("xs",  producerXS)
             ]
  where producerN0 = mkProducerSet [(cNil,  ["n0"])]
        producerN1 = mkProducerSet [(cCons, ["n1"])]
        producerXS = producerN0

pointerInNodeSpec :: ProducerMap -> Spec
pointerInNodeSpec found = it "pointer_in_node" $ found `sameAs` pointerInNodeExpected



undefinedSrc :: FilePath
undefinedSrc = cbyExamples </> "undefined.grin"

undefinedExpected :: ProducerMap
undefinedExpected = ProducerMap $
  M.fromList [ ("n0",  producerN0)
             , ("n1",  producerN1)
             , ("n2",  producerN2)
             , ("p0",  emptyProducerSet)
             , ("p1",  emptyProducerSet)
             , ("p2",  emptyProducerSet)
             , ("x0",  emptyProducerSet)
             ]
  where producerN0 = mkProducerSet [(cCons, [udProd])]
        producerN1 = mkProducerSet [(cCons, [udProd]), (cNil, [udProd])]
        producerN2 = mkProducerSet [(cCons, ["n2"])]

undefinedSpec :: ProducerMap -> Spec
undefinedSpec found = it "undefined" $ found `sameAs` undefinedExpected


indexedStoreSrc :: FilePath
indexedStoreSrc = cbyExamples </> "indexed_store.grin"

indexedStoreExpected :: ProducerMap
indexedStoreExpected = ProducerMap $
  M.fromList [ ("n0", producerN0)
             , ("n1", producerN1)
             , ("n2", producerN2)
             , ("p0", emptyProducerSet)
             , ("p1", emptyProducerSet)
             , ("p2", emptyProducerSet) 
             , ("y2", producerN2)
             ]
  where producerN0 = mkProducerSet [(cInt,  ["n0"])]
        producerN1 = mkProducerSet [(cBool, ["n1"])]
        producerN2 = mkProducerSet [(cWord, ["n2"])]
    

indexedStoreSpec :: ProducerMap -> Spec
indexedStoreSpec found = it "indexed_store" $ found `sameAs` indexedStoreExpected
module LiveVariable.Tests.IndexedStore where

import Data.Map    (Map)
import Data.Vector (Vector)

import qualified Data.Map    as M
import qualified Data.Vector as V

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import AbstractInterpretation.LiveVariable hiding (live)
import AbstractInterpretation.LVAResult

import LiveVariable.Tests.Util


indexedStoreSrc :: FilePath
indexedStoreSrc = lvaExamples </> "indexed_store.grin"

indexedStoreSpec :: LVAResult -> Spec
indexedStoreSpec found = it "indexed_store" $ found `sameAs` indexedStoreExpected

indexedStoreExpected :: LVAResult
indexedStoreExpected = LVAResult
  { _memory   = indexedStoreExpectedHeap
  , _register = indexedStoreExpectedRegisters
  , _function = indexedStoreExpectedFunctions
  }

indexedStoreExpectedHeap :: Vector Liveness
indexedStoreExpectedHeap = V.fromList 
  [ livenessN0
  , livenessN1
  , livenessN2
  ]

indexedStoreExpectedRegisters :: Map Name Liveness
indexedStoreExpectedRegisters = M.fromList
  [ ("p0", deadVal)
  , ("p1", deadVal)
  , ("p2", deadVal)
  , ("n0", livenessN0)
  , ("n1", livenessN1)
  , ("n2", livenessN2)
  ]

indexedStoreExpectedFunctions :: Map Name (Liveness, Vector Liveness)
indexedStoreExpectedFunctions = mkFunctionLivenessMap []

livenessN0, livenessN1, livenessN2  :: Liveness
livenessN0 = nodeSet [ (cInt,  [dead]) ]
livenessN1 = nodeSet [ (cBool, [dead]) ]
livenessN2 = nodeSet [ (cWord, [dead]) ]

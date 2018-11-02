module HeapPointsTo.Tests.IndexedStore where 

import System.FilePath

import Data.Map (Map)
import Data.Set (Set)
import Data.Vector (Vector)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Test.Util
import Test.Hspec
import Test.Assertions

import Grin.Grin
import AbstractInterpretation.HPTResult

import HeapPointsTo.Tests.Util

indexedStoreSrc :: FilePath
indexedStoreSrc = hptExamples </> "indexed_store.grin"

indexedStoreSpec :: HPTResult -> Spec 
indexedStoreSpec found = it "undefined" $ found `sameAs` indexedStoreExpected

indexedStoreExpected :: HPTResult
indexedStoreExpected = HPTResult
  { _memory   = indexedStoreExpectedHeap
  , _register = indexedStoreExpectedRegisters
  , _function = indexedStoreExpectedFunctions
  }

nodeSetN0, nodeSetN1, nodeSetN2 :: NodeSet
nodeSetN0 = mkNodeSet [(cInt,  [[T_Int64]])]
nodeSetN1 = mkNodeSet [(cBool, [[T_Int64]])]
nodeSetN2 = mkNodeSet [(cWord, [[T_Int64]])]

locTP0, locTP1, locTP2 :: SimpleType
locTP0 = locT 0
locTP1 = locT 1
locTP2 = locT 2

indexedStoreExpectedHeap :: Vector NodeSet
indexedStoreExpectedHeap = V.fromList 
  [ nodeSetN0
  , nodeSetN1
  , nodeSetN2
  ]

indexedStoreExpectedRegisters :: Map Name TypeSet 
indexedStoreExpectedRegisters = M.fromList
  [ ("p0", loc 0)
  , ("p1", loc 1)
  , ("p2", loc 2)
  , ("n0", tySetFromNodeSet nodeSetN0)
  , ("n1", tySetFromNodeSet nodeSetN1)
  , ("n2", tySetFromNodeSet nodeSetN2)
  ]

indexedStoreExpectedFunctions :: Map Name (TypeSet, Vector TypeSet)
indexedStoreExpectedFunctions = M.singleton "grinMain" (mkSimpleMain T_Int64)
module DeadCodeElimination.Tests.DeadData.OnlyDummify where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadData.Util

(onlyDummifyBefore, onlyDummifyAfter, onlyDummifySpec) = mkDDETestCase "only_dummify"
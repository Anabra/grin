{-# LANGUAGE QuasiQuotes #-}
module Transformations.Simplifying.StoreIndexingSpec where 

import Transformations.Simplifying.StoreIndexing

import Test.Test
import Grin.TH
import Grin.Grin
import Test.Hspec
import Test.Assertions


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  it "simple" $ do
    let before = [prog|
      grinMain =
        p0 <- store (CInt 5)
        p1 <- store (CInt 5)
        p2 <- store (CInt 5)
        pure 0
      |]
    let after = [prog|
      grinMain =
        p0 <- store[0] (CInt 5)
        p1 <- store[1] (CInt 5)
        p2 <- store[2] (CInt 5)
        pure 0
      |]
    (storeIndexing before) `sameAs` after

  it "indexed" $ do
    let before = [prog|
      grinMain =
        p0 <- store[0] (CInt 5)
        p1 <- store[1] (CInt 5)
        p2 <- store    (CInt 5)
        pure 0
      |]
    let after = [prog|
      grinMain =
        p0 <- store[0] (CInt 5)
        p1 <- store[1] (CInt 5)
        p2 <- store[2] (CInt 5)
        pure 0
      |]
    (storeIndexing before) `sameAs` after

  it "sparsely_indexed" $ do
    let before = [prog|
      grinMain =
        p0 <- store[0] (CInt 5)
        p1 <- store    (CInt 5)
        p2 <- store[2] (CInt 5)
        pure 0
      |]
    let after = [prog|
      grinMain =
        p0 <- store[0] (CInt 5)
        p1 <- store[3] (CInt 5)
        p2 <- store[2] (CInt 5)
        pure 0
      |]
    (storeIndexing before) `sameAs` after


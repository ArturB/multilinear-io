{-|
Module      : Bench
Description : Benchmark of Multilinear library
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

-}

module Main (
    main
) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Criterion.Main
import           Multilinear.Generic
import           Multilinear.Generic.Serialize
import qualified Multilinear.Matrix         as Matrix

gen :: Int -> Int -> Double
gen j k = sin (fromIntegral j) + cos (fromIntegral k)

writeMatrixBinaryBench :: Int -> Benchmark
writeMatrixBinaryBench s = 
    bench (show s ++ "x" ++ show s) $ 
        nfIO $ toBinaryFile (Matrix.fromIndices "ij" s s gen) ("benchmark/matrix-" ++ show s ++ ".zlib")

readMatrixBinaryBench :: Int -> Benchmark
readMatrixBinaryBench s = do
    let tensorDoubleIOT = fromBinaryFile ("benchmark/matrix-" ++ show s ++ ".zlib") :: ExceptT String IO (Tensor Double)
    bench (show s ++ "x" ++ show s) $ 
        nfIO $ runExceptT tensorDoubleIOT

writeMatrixJSONBench :: Int -> Benchmark
writeMatrixJSONBench s = 
    bench (show s ++ "x" ++ show s) $ 
        nfIO $ toJSONFile (Matrix.fromIndices "ij" s s gen) ("benchmark/matrix-" ++ show s ++ ".json")

readMatrixJSONBench :: Int -> Benchmark
readMatrixJSONBench s = do
    let tensorDoubleIOT = fromJSONFile ("benchmark/matrix-" ++ show s ++ ".json") :: MaybeT IO (Tensor Double)
    bench (show s ++ "x" ++ show s) $ 
        nfIO $ runMaybeT tensorDoubleIOT


main :: IO ()
main = defaultMain [
    bgroup "matrix binary write" $ writeMatrixBinaryBench <$> [100, 200, 400, 800, 1600],
    bgroup "matrix binary read"  $ readMatrixBinaryBench  <$> [100, 200, 400, 800, 1600],
    bgroup "matrix JSON write"   $ writeMatrixJSONBench   <$> [100, 200, 400, 800, 1600],
    bgroup "matrix JSON read"    $ readMatrixJSONBench    <$> [100, 200, 400, 800, 1600]
    ]

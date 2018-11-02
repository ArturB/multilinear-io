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
import           Criterion.Main
import           Multilinear.Generic
import           Multilinear.Generic.Serialize
import qualified Multilinear.Matrix         as Matrix

gen :: Int -> Int -> Double
gen j k = sin (fromIntegral j) + cos (fromIntegral k)

writeMatrixBench :: Int -> Benchmark
writeMatrixBench s = 
    bench ((show s) ++ "x" ++ (show s)) $ 
        nfIO $ toBinaryFile (Matrix.fromIndices "ij" s s gen) ("benchmark/binary-write-matrix-" ++ show s ++ ".csv")

readMatrixBench :: Int -> Benchmark
readMatrixBench s = do
    let tensorDoubleIOT = fromBinaryFile ("benchmark/binary-write-matrix-" ++ show s ++ ".csv") :: ExceptT String IO (Tensor Double)
    bench ((show s) ++ "x" ++ (show s)) $ 
        nfIO $ runExceptT tensorDoubleIOT


main :: IO ()
main = defaultMain [
    bgroup "matrix binary write" $ writeMatrixBench <$> [100, 200, 400, 800, 1600, 3200],
    bgroup "matrix binary read"  $ readMatrixBench  <$> [100, 200, 400, 800, 1600, 3200]
    ]

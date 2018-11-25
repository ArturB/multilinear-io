module Main (
  main
) where

import           Control.Monad.Trans.Maybe
import qualified Multilinear.Generic.MultiCore  as MultiCore
import qualified Multilinear.Generic.Sequential as Sequential
import qualified Multilinear.Matrix         as Matrix
import           System.Directory
import           Test.MultiCore
import           Test.Sequential

fileName1 :: String
fileName1  = "test/m1"

m1MC :: MultiCore.Tensor Double
m1MC = Matrix.fromIndices "ij" 50 50 $ \i j -> cos (fromIntegral i) + sin (fromIntegral j)

m1Seq :: Sequential.Tensor Double
m1Seq = Matrix.fromIndices "ij" 50 50 $ \i j -> cos (fromIntegral i) + sin (fromIntegral j)

-- ENTRY POINT
main :: IO ()
main = do
  putStrLn "Testing MultiCore..."
  Test.MultiCore.writeMatrixJSON m1MC fileName1
  runMaybeT $ Test.MultiCore.readMatrixJSON m1MC fileName1
  removeFile $ fileName1 ++ ".json"
  putStrLn "Testing Sequential..."
  Test.Sequential.writeMatrixJSON m1Seq fileName1
  runMaybeT $ Test.Sequential.readMatrixJSON m1Seq fileName1
  removeFile $ fileName1 ++ ".json"
  return ()

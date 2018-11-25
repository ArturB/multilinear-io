module Main (
  main
) where

import           Control.Monad.Trans.Except
import qualified Multilinear.Generic.MultiCore  as MultiCore
import qualified Multilinear.Generic.Sequential as Sequential
import qualified Multilinear.Matrix             as Matrix
import qualified Multilinear.Vector             as Vector
import           System.Directory
import           Test.MultiCore
import           Test.Sequential

matrixSeq :: Sequential.Tensor Double
matrixSeq = Matrix.fromIndices "ij" 50 50 $ \i j -> cos (fromIntegral i) + sin (fromIntegral j)

vectorSeq :: Sequential.Tensor Double
vectorSeq = Vector.fromIndices "i" 50 $ \i -> cos (fromIntegral i)

formSeq :: Sequential.Tensor Double
formSeq = Vector.fromIndices "i" 50 $ \i -> cos (fromIntegral i)

scalarSeq :: Sequential.Tensor Double
scalarSeq = Sequential.Scalar 5.0

matrixMC :: MultiCore.Tensor Double
matrixMC = Matrix.fromIndices "ij" 50 50 $ \i j -> cos (fromIntegral i) + sin (fromIntegral j)

vectorMC :: MultiCore.Tensor Double
vectorMC = Vector.fromIndices "i" 50 $ \i -> cos (fromIntegral i)

formMC :: MultiCore.Tensor Double
formMC = Vector.fromIndices "i" 50 $ \i -> cos (fromIntegral i)

scalarMC :: MultiCore.Tensor Double
scalarMC = MultiCore.Scalar 5.0

-- ENTRY POINT
main :: IO ()
main = do
  -- TEST MULTICORE
  putStrLn "Testing MultiCore..."
  -- Scalar CSV read/write
  Test.MultiCore.writeScalarCSV scalarMC "test/s1"
  runExceptT $ Test.MultiCore.readScalarCSV scalarMC "test/s1"
  -- Vector CSV read/write
  Test.MultiCore.writeVectorCSV vectorMC "test/v1"
  runExceptT $ Test.MultiCore.readVectorCSV vectorMC "test/v1"
  -- Form CSV read/write
  Test.MultiCore.writeFormCSV formMC "test/f1"
  runExceptT $ Test.MultiCore.readFormCSV formMC "test/f1"
  -- Matrix CSV read/write
  Test.MultiCore.writeMatrixCSV matrixMC "test/m1"
  runExceptT $ Test.MultiCore.readMatrixCSV matrixMC "test/m1"
  -- Remove test file
  removeFile "test/s1.csv"
  removeFile "test/v1.csv"
  removeFile "test/f1.csv"
  removeFile "test/m1.csv"

  -- TEST SEQUENTIAL
  putStrLn "Testing Sequential..."
  -- Scalar CSV read/write
  Test.Sequential.writeScalarCSV scalarSeq "test/s1"
  runExceptT $ Test.Sequential.readScalarCSV scalarSeq "test/s1"
  -- Vector CSV read/write
  Test.Sequential.writeVectorCSV vectorSeq "test/v1"
  runExceptT $ Test.Sequential.readVectorCSV vectorSeq "test/v1"
  -- Form CSV read/write
  Test.Sequential.writeFormCSV formSeq "test/f1"
  runExceptT $ Test.Sequential.readFormCSV formSeq "test/f1"
  -- Matrix CSV read/write
  Test.Sequential.writeMatrixCSV matrixSeq "test/m1"
  runExceptT $ Test.Sequential.readMatrixCSV matrixSeq "test/m1"
  -- Remove test file
  removeFile "test/s1.csv"
  removeFile "test/v1.csv"
  removeFile "test/f1.csv"
  removeFile "test/m1.csv"
  return ()

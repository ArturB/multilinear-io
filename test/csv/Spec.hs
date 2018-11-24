module Main (
  main
) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.Maybe
import           Multilinear.Class
import           Multilinear.Generic
import           Multilinear.Generic.Serialize
import           Multilinear.Index
import qualified Multilinear.Matrix         as Matrix
import qualified Multilinear.Vector         as Vector
import           System.Directory

matrix :: Tensor Double
matrix = Matrix.fromIndices "ij" 50 50 $ \i j -> cos (fromIntegral i) + sin (fromIntegral j)

vector :: Tensor Double
vector = Vector.fromIndices "i" 50 $ \i -> cos (fromIntegral i)

form :: Tensor Double
form = Vector.fromIndices "i" 50 $ \i -> cos (fromIntegral i)

scalar :: Tensor Double
scalar = Scalar 5.0

writeScalarCSV :: Tensor Double -> String -> IO ()
writeScalarCSV m fileName = do
  putStrLn $ "Writing scalar to " ++ fileName ++ ".csv..."
  m `toCSVFile` (fileName ++ ".csv")
  putStrLn "Scalar successfully written!"

readScalarCSV :: Tensor Double -> String -> ExceptT String IO ()
readScalarCSV m fileName = do
  lift $ putStrLn $ "Reading scalar s1 from " ++ fileName ++ ".csv..."
  (m1' :: Tensor Double) <- fromCSVFile (fileName ++ ".csv") ',' "ij"
  if m1' == m then
    lift $ putStrLn "Scalar successfully read!"
  else
    ExceptT $ return $ Left "Scalar deserialization error"

writeVectorCSV :: Tensor Double -> String -> IO ()
writeVectorCSV m fileName = do
  let size = fromJust . indexSize <$> head $ indices m
  putStrLn $ "Writing " ++ show size ++ " elements vector to " ++ fileName ++ ".csv..."
  m `toCSVFile` (fileName ++ ".csv")
  putStrLn "Vector successfully written!"

readVectorCSV :: Tensor Double -> String -> ExceptT String IO ()
readVectorCSV m fileName = do
  lift $ putStrLn $ "Reading vector v1 from " ++ fileName ++ ".csv..."
  (m1' :: Tensor Double) <- fromCSVFile (fileName ++ ".csv") ',' "ij"
  if m1' == m then
    lift $ putStrLn "Vector successfully read!"
  else
    ExceptT $ return $ Left "Vector deserialization error"

writeFormCSV :: Tensor Double -> String -> IO ()
writeFormCSV m fileName = do
  let size = fromJust . indexSize <$> head $ indices m
  putStrLn $ "Writing " ++ show size ++ " elements form to " ++ fileName ++ ".csv..."
  m `toCSVFile` (fileName ++ ".csv")
  putStrLn "Form successfully written!"

readFormCSV :: Tensor Double -> String -> ExceptT String IO ()
readFormCSV m fileName = do
  lift $ putStrLn $ "Reading form f1 from " ++ fileName ++ ".csv..."
  (m1' :: Tensor Double) <- fromCSVFile (fileName ++ ".csv") ',' "ij"
  if m1' == m then
    lift $ putStrLn "Form successfully read!"
  else
    ExceptT $ return $ Left "Form deserialization error"

writeMatrixCSV :: Tensor Double -> String -> IO ()
writeMatrixCSV m fileName = do
  let [xsize,ysize] = indexSize <$> indices m
  putStrLn $ "Writing " ++ show (fromJust xsize) ++ "x" ++ show (fromJust ysize) ++ " matrix to " ++ fileName ++ ".csv..."
  m `toCSVFile` (fileName ++ ".csv")
  putStrLn "Matrix successfully written!"

readMatrixCSV :: Tensor Double -> String -> ExceptT String IO ()
readMatrixCSV m fileName = do
  lift $ putStrLn $ "Reading matrix m1 from " ++ fileName ++ ".csv..."
  (m1' :: Tensor Double) <- fromCSVFile (fileName ++ ".csv") ',' "ij"
  if m1' == m then
    lift $ putStrLn "Matrix successfully read!"
  else
    ExceptT $ return $ Left "Matrix deserialization error"

  
-- ENTRY POINT
main :: IO ()
main = do
  -- Scalar CSV read/write
  writeScalarCSV scalar "test/s1"
  runExceptT $ readScalarCSV scalar "test/s1"
  -- Vector CSV read/write
  writeVectorCSV vector "test/v1"
  runExceptT $ readVectorCSV vector "test/v1"
  -- Form CSV read/write
  writeFormCSV form "test/f1"
  runExceptT $ readFormCSV form "test/f1"
  -- Matrix CSV read/write
  writeMatrixCSV matrix "test/m1"
  runExceptT $ readMatrixCSV matrix "test/m1"
  -- Remove test file
  removeFile "test/s1.csv"
  removeFile "test/v1.csv"
  removeFile "test/f1.csv"
  removeFile "test/m1.csv"
  return ()

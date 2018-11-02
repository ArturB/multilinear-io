module Main (
  main
) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Multilinear.Class
import           Multilinear.Generic
import           Multilinear.Generic.Serialize
import           Multilinear.Index
import qualified Multilinear.Matrix         as Matrix

fileName1 :: String
fileName1  = "test/m1"

m1 :: Tensor Double
m1 = Matrix.fromIndices "ij" 500 500 $ \i j -> cos (fromIntegral i) + sin (fromIntegral j)

writeMatrixBinary :: Tensor Double -> String -> IO ()
writeMatrixBinary m fileName = do
  let [xsize,ysize] = indexSize <$> indices m1
  putStrLn $ "Writing " ++ show xsize ++ "x" ++ show ysize ++ " matrix to " ++ fileName ++ ".zlib..."
  m `toBinaryFile` (fileName ++ ".zlib")
  putStrLn $ "Matrix successfully written!"

writeMatrixJSON :: Tensor Double -> String -> IO ()
writeMatrixJSON m fileName = do
  let [xsize,ysize] = indexSize <$> indices m1
  putStrLn $ "Writing " ++ show xsize ++ "x" ++ show ysize ++ " matrix to " ++ fileName ++ ".json..."
  m `toJSONFile` (fileName ++ ".json")
  putStrLn $ "Matrix successfully written!"

readMatrixBinary :: String -> ExceptT String IO ()
readMatrixBinary fileName = do
  lift $ putStrLn $ "Reading matrix m1 from " ++ fileName ++ ".zlib..."
  m1' <- fromBinaryFile (fileName ++ ".zlib")
  if m1' == m1 then
    lift $ putStrLn "Matrix successfully read!"
  else
    ExceptT $ return $ Left "Matrix deserialization error"

readMatrixJSON :: String -> MaybeT IO ()
readMatrixJSON fileName = do
  lift $ putStrLn $ "Reading matrix m1 from " ++ fileName ++ ".json..."
  m1' <- fromJSONFile (fileName ++ ".json")
  if m1' == m1 then
    lift $ putStrLn "Matrix successfully read!"
  else
    MaybeT $ return Nothing

-- ENTRY POINT
main :: IO ()
main = do
  writeMatrixBinary m1 fileName1
  runExceptT $ readMatrixBinary fileName1
  writeMatrixJSON m1 fileName1
  runMaybeT $ readMatrixJSON fileName1
  return ()

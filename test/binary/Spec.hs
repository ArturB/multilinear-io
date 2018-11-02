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
import           System.Directory

fileName1 :: String
fileName1  = "test/m1"

m1 :: Tensor Double
m1 = Matrix.fromIndices "ij" 500 500 $ \i j -> cos (fromIntegral i) + sin (fromIntegral j)

writeMatrixBinary :: Tensor Double -> String -> IO ()
writeMatrixBinary m fileName = do
  let [xsize,ysize] = indexSize <$> indices m1
  putStrLn $ "Writing " ++ show (fromJust xsize) ++ "x" ++ show (fromJust ysize) ++ " matrix to " ++ fileName ++ ".zlib..."
  m `toBinaryFile` (fileName ++ ".zlib")
  putStrLn $ "Matrix successfully written!"

readMatrixBinary :: String -> ExceptT String IO ()
readMatrixBinary fileName = do
  lift $ putStrLn $ "Reading matrix m1 from " ++ fileName ++ ".zlib..."
  m1' <- fromBinaryFile (fileName ++ ".zlib")
  if m1' == m1 then
    lift $ putStrLn "Matrix successfully read!"
  else
    ExceptT $ return $ Left "Matrix deserialization error"

-- ENTRY POINT
main :: IO ()
main = do
  writeMatrixBinary m1 fileName1
  runExceptT $ readMatrixBinary fileName1
  removeFile $ fileName1 ++ ".zlib"
  return ()

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

writeMatrixCSV :: Tensor Double -> String -> IO ()
writeMatrixCSV m fileName = do
  let [xsize,ysize] = indexSize <$> indices m1
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
  writeMatrixCSV m1 fileName1
  runExceptT $ readMatrixCSV m1 fileName1
  removeFile $ fileName1 ++ ".csv"
  return ()

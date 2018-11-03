module Main (
  main
) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
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
m1 = Matrix.fromIndices "ij" 50 50 $ \i j -> cos (fromIntegral i) + sin (fromIntegral j)

writeMatrixJSON :: Tensor Double -> String -> IO ()
writeMatrixJSON m fileName = do
  let [xsize,ysize] = indexSize <$> indices m1
  putStrLn $ "Writing " ++ show (fromJust xsize) ++ "x" ++ show (fromJust ysize) ++ " matrix to " ++ fileName ++ ".json..."
  m `toJSONFile` (fileName ++ ".json")
  putStrLn $ "Matrix successfully written!"

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
  writeMatrixJSON m1 fileName1
  runMaybeT $ readMatrixJSON fileName1
  removeFile $ fileName1 ++ ".json"
  return ()

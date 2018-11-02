module Main (
  main
) where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Class
import qualified Multilinear.Matrix         as Matrix
import           Multilinear.Class
import           Multilinear.Index
import           Multilinear.Generic
import           Multilinear.Generic.Serialize

fileName1 :: String
fileName1  = "test/m1.csv"

m1 :: Tensor Double
m1 = Matrix.fromIndices "ij" 1000 1000 $ \i j -> cos (fromIntegral i) + sin (fromIntegral j)

writeMatrix :: Tensor Double -> String -> IO ()
writeMarix m fileName = do
  let [xsize,ysize] = indexSize <$> indices m1
  putStrLn $ "Writing " ++ show xsize ++ "x" ++ show ysize ++ " matrix to " ++ fileName ++ "..."
  m `toBinaryFile` fileName
  putStrLn $ "Matrix successfully written!"

readMatrix :: String -> ExceptT String IO ()
readMatrix fileName = do
  lift $ putStrLn $ "Reading matrix m1 from " ++ fileName ++ "..."
  m1' <- fromBinaryFile fileName
  if m1' == m1 then
    lift $ putStrLn "Matrix successfully read!"
  else
    ExceptT $ return $ Left "Matrix deserialization error"

-- ENTRY POINT
main :: IO (Either String ())
main = do
  writeMatrix m1 fileName1
  runExceptT $ readMatrix fileName1

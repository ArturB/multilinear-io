{-|
Module      : Multilinear.Generic.Serialize
Description : Generic array tensor serialization: binary, JSON, CSV. 
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

-}

module Multilinear.Generic.Serialize (
    toBinary, toBinaryFile,
    fromBinary, fromBinaryFile,
    Multilinear.Generic.Serialize.toJSON, toJSONFile,
    Multilinear.Generic.Serialize.fromJSON, fromJSONFile,
    --fromCSV, toCSV
) where

import           Codec.Compression.GZip
import           Conduit
import           Control.Exception
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import qualified Data.ByteString.Lazy       as ByteString
import           Data.Either
import           Data.Serialize
import qualified Data.Vector                as Boxed
import           Data.Vector.Serialize      ()
import           Multilinear.Class
import           Multilinear.Generic
import qualified Multilinear.Index.Finite   as Finite
import           Multilinear.Index.Finite.Serialize ()
import           Multilinear.Index.Infinite.Serialize ()

-- Binary serialization instance
instance Serialize a => Serialize (Tensor a)
-- JSON serialization instance
instance ToJSON a => ToJSON (Tensor a)
-- JSON deserialization instance
instance FromJSON a => FromJSON (Tensor a)

invalidIndices :: String -- ^ CSV error message
invalidIndices = "Indices and its sizes not compatible with structure of matrix!"

deserializationError :: String -- ^ CSV error message
deserializationError = "Components deserialization error!"

{-| Serialize tensor to zlib compressed binary string -}
toBinary :: (
    Serialize a 
  ) => Tensor a              -- ^ Tensor to serialize
    -> ByteString.ByteString -- ^ Tensor serialized to lazy ByteString
toBinary = compress . Data.Serialize.encodeLazy

{-| Write tensor to binary file. Uses compression with gzip -}
toBinaryFile :: (
    Serialize a 
  ) => String    -- ^ File name
    -> Tensor a  -- ^ Tensor to serialize
    -> IO ()
toBinaryFile fileName = 
  runConduitRes $ 
    sourceLazy . bs .| sinkFile fileName

{-| Deserialize tensor from zlib compressed binary string -}
fromBinary :: (
    Serialize a
  ) => ByteString.ByteString    -- ^ ByteString to deserialize
    -> Either String (Tensor a) -- ^ Deserialized tensor or an error message. 
fromBinary = Data.Serialize.decodeLazy . decompress

{-| Read tensor from binary file -}
fromBinaryFile :: (
    Serialize a
  ) => String                      -- ^ File path. 
    -> Except String IO (Tensor a) -- ^ Deserialized tensor or an error message
fromBinaryFile fileName = do
  contents <- lift $ runConduitRes $ sourceFile fileName .| sinkLazy
  except $ fromBinary contents

{-| Serialize tensor to JSON string -}
toJSON :: (
    ToJSON a
  ) => Tensor a              -- ^ Tensor to serialize. 
    -> ByteString.ByteString -- ^ Tensor serialized to lazy ByteString. 
toJSON = Data.Aeson.encode

{-| Write tensor to JSON file -}
toJSONFile :: (
    ToJSON a
  ) => String   -- ^ File path. 
    -> Tensor a -- ^ Tensor to serialize
    -> IO ()
toJSONFile name = ByteString.writeFile name . Multilinear.Generic.Serialize.toJSON

{-| Deserialize tensor from JSON string -}
fromJSON :: (
    FromJSON a
  ) => ByteString.ByteString -- ^ ByteString to deserialize
    -> Maybe (Tensor a)      -- ^ Deserialized tensor or Nothing, if deserialization error occured. 
fromJSON = Data.Aeson.decode

{-| Read tensor from JSON file -}
fromJSONFile :: (
    FromJSON a
  ) => String               -- ^ File path. 
    -> MaybeT IO (Tensor a) -- ^ Deserialized tensor or Nothing, if error occured. 
fromJSONFile name = do
    contents <- lift $ ByteString.readFile name
    MaybeT $ return $ Multilinear.Generic.Serialize.fromJSON contents

{-| Read tensor (matrix) components from CSV file. -}
--{-# INLINE fromCSV #-}
{-fromCSV :: (
    Num a, Serialize a
  ) => String                                  -- ^ Indices names (one character per index, first character: rows index, second character: columns index)
    -> String                                  -- ^ CSV file name
    -> Char                                    -- ^ Separator expected to be used in this CSV file
    -> EitherT SomeException IO (Tensor a)     -- ^ Generated matrix or error message

fromCSV x = case x of
  [u,d] -> \fileName separator -> do
    csv <- EitherT $ readCSVFile (CSVS separator (Just '"') (Just '"') separator) fileName
    let components = (Data.Serialize.decode <$> ) <$> csv
    let rows = length components
    let columns = if rows > 0 then length $ rights (head components) else 0
    if rows > 0 && columns > 0
    then return $ 
      FiniteTensor (Finite.Contravariant rows [u]) $ (
        SimpleFinite (Finite.Covariant columns [d]) . Boxed.fromList . rights
      ) <$> Boxed.fromList components
    else EitherT $ return $ Left $ SomeException $ TypeError deserializationError

  _ -> \_ _ -> return $ Err invalidIndices
-}

{-| Write matrix to CSV file. -}
--{-# INLINE toCSV                    #-}
{-toCSV :: (
    Num a, Serialize a
  ) => Tensor a  -- ^ Matrix to serialize. If given tensor os not a matrix, an error occurs and no data (0 rows) are saved to file. 
    -> String    -- ^ CSV file name
    -> Char      -- ^ Separator expected to be used in this CSV file
    -> IO Int    -- ^ Number of rows written

toCSV t = case order t of
  (1,1) -> \fileName separator ->
    let t' = _standardize t
        elems = Boxed.toList $ Boxed.toList . tensorScalars <$> tensorsFinite t'
        encodedElems = (Data.Serialize.encode <$>) <$> elems
    in  writeCSVFile (CSVS separator (Just '"') (Just '"') separator) fileName encodedElems

  _ -> \_ _ -> return 0
-}

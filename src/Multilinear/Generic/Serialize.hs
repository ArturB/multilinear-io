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
    Multilinear.Generic.Serialize.fromJSON, fromJSONFile
) where

import           Codec.Compression.GZip
import           Control.Exception
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import qualified Data.ByteString.Lazy       as ByteString
import           Data.CSV.Enumerator
import           Data.Either
import           Data.Serialize
import qualified Data.Vector                as Boxed
import           Data.Vector.Serialize      ()
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

{-| Serialize tensor to binary string -}
toBinary :: (
    Serialize a 
  ) => Tensor a              -- ^ Tensor to serialize
    -> ByteString.ByteString -- ^ Tensor serialized to lazy ByteString
toBinary = Data.Serialize.encodeLazy

{-| Write tensor to binary file. Uses compression with gzip -}
toBinaryFile :: (
    Serialize a 
  ) => String    -- ^ File name
    -> Tensor a  -- ^ Tensor to serialize
    -> IO ()
toBinaryFile name = ByteString.writeFile name . compress . toBinary

{-| Deserialize tensor from binary string -}
fromBinary :: (
    Serialize a
  ) => ByteString.ByteString    -- ^ ByteString to deserialize
    -> Either String (Tensor a) -- ^ Deserialized tensor or an error message. 
fromBinary = Data.Serialize.decodeLazy

{-| Read tensor from binary file -}
fromBinaryFile :: (
    Serialize a
  ) => String                       -- ^ File path. 
    -> EitherT String IO (Tensor a) -- ^ Deserialized tensor or an error message
fromBinaryFile name = do
    contents <- lift $ ByteString.readFile name
    EitherT $ return $ fromBinary $ decompress contents

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
{-# INLINE fromCSV #-}
fromCSV :: (
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

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
    Multilinear.Generic.Serialize.toJSON, 
    Multilinear.Generic.Serialize.toJSONFile,
    Multilinear.Generic.Serialize.fromJSON, 
    Multilinear.Generic.Serialize.fromJSONFile
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
import           Multilinear.Index.Finite ()
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
  ) => Tensor a  -- ^ Tensor to serialize
    -> String    -- ^ File name
    -> IO ()
toBinaryFile t fileName = do
  let bs = toBinary t
  runConduitRes $ 
    sourceLazy bs .| sinkFile fileName

{-| Deserialize tensor from zlib compressed binary string -}
fromBinary :: (
    Serialize a
  ) => ByteString.ByteString    -- ^ ByteString to deserialize
    -> Either String (Tensor a) -- ^ Deserialized tensor or an error message. 
fromBinary = Data.Serialize.decodeLazy . decompress

{-| Read tensor from binary file -}
fromBinaryFile :: (
    Serialize a
  ) => String                       -- ^ File path. 
    -> ExceptT String IO (Tensor a) -- ^ Deserialized tensor or an error message
fromBinaryFile fileName = do
  contents <- lift $ runConduitRes $ 
    sourceFile fileName .| sinkLazy
  ExceptT $ return $ fromBinary contents

{-| Serialize tensor to JSON string -}
toJSON :: (
    ToJSON a
  ) => Tensor a              -- ^ Tensor to serialize. 
    -> ByteString.ByteString -- ^ Tensor serialized to lazy ByteString. 
toJSON = Data.Aeson.encode

{-| Write tensor to JSON file -}
toJSONFile :: (
    ToJSON a
  ) => Tensor a -- ^ Tensor to serialize
    -> String   -- ^ File path. 
    -> IO ()
toJSONFile t fileName = do
  let bs = Multilinear.Generic.Serialize.toJSON t
  runConduitRes $ 
    sourceLazy bs .| sinkFile fileName

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
fromJSONFile fileName = do
  contents <- lift $ runConduitRes $ 
    sourceFile fileName .| sinkLazy
  MaybeT $ return $ Multilinear.Generic.Serialize.fromJSON contents

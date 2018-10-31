{-|
Module      : Multilinear.Index.Infinite.Serialize
Description : Infinite-dimensional tensor index serialization: binary, JSON. 
Copyright   : (c) Artur M. Brodzki, 2018
License     : BSD3
Maintainer  : artur@brodzki.org
Stability   : experimental
Portability : Windows/POSIX

Infinite-dimensional tensor index.

-}

module Multilinear.Index.Infinite.Serialize (
    Index(..)
) where

import           Data.Aeson
import           Data.Serialize
import           Multilinear.Index.Infinite

{-| Binary serialization and deserialization |-}
instance Serialize Index

{-| Serialization to and from JSON |-}
instance FromJSON Index
instance   ToJSON Index

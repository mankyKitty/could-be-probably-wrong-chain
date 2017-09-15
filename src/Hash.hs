{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
module Hash where

import           Control.Lens           (re, (^.), _Show, _Wrapped)

import           Control.Category       ((.))

import           Data.Function          (($))
import           GHC.Show               (show)

import           Data.ByteString        (ByteString)
import           Data.ByteString.Lens   (packedChars)

import           Data.Semigroup         ((<>))
import qualified Data.Text              as Text

import           Crypto.Hash            (Digest, hash)
import           Crypto.Hash.Algorithms (SHA256)

import           Types                  (Block, BlockIndex, Hash (Hash),
                                         Timestamp, blockData, blockHash,
                                         blockIndex, blockTimestamp)

class AsHashable a where
  _AsHashable :: a -> ByteString

calculateHash
  :: AsHashable a
  => BlockIndex
  -> Hash
  -> Timestamp
  -> a
  -> Hash
calculateHash bi ph ts a =
  let toBS = (^. _Wrapped . re _Show . packedChars)
  in
    sha256
    $  (toBS bi)
    <> (toBS ph)
    <> (toBS ts)
    <> _AsHashable a

calculateHashForBlock
  :: AsHashable a
  => Block a
  -> Hash
calculateHashForBlock b =
  calculateHash
  (b ^. blockIndex)
  (b ^. blockHash)
  (b ^. blockTimestamp)
  (b ^. blockData)

sha256
  :: ByteString
  -> Hash
sha256 bs =
  let hished = ( hash bs ) :: Digest SHA256
  in
    ( Hash . Text.pack . show ) hished

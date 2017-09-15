{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Types where

import           Control.Category ((.))
import           Control.Lens     (makeLenses, makeWrapped, to, (^.), _Wrapped)
import           GHC.Show         (Show)
import           Prelude          (Eq, (+))

import           Numeric.Natural  (Natural)

import           Data.Sequence    (Seq)

import           Data.Time        (UTCTime)

import           Data.Text        (Text)

data Error
  = InvalidIndex
  | InvalidCurrentHash
  | InvalidPreviousHash
  | NewChainEmpty
  | OldChainLongerThanNew
  | EmptyBlockChain

newtype BlockIndex = BlockIndex Natural
  deriving Show
makeWrapped ''BlockIndex

newtype PreviousHash = PreviousHash Text
  deriving Show
makeWrapped ''PreviousHash

newtype Hash = Hash Text
  deriving ( Show, Eq )
makeWrapped ''Hash

newtype Timestamp = Timestamp UTCTime
  deriving Show
makeWrapped ''Timestamp

data Block a = Block
  { _blockIndex        :: BlockIndex
  , _blockPreviousHash :: PreviousHash
  , _blockTimestamp    :: Timestamp
  , _blockData         :: a
  , _blockHash         :: Hash
  } deriving Show
makeLenses ''Block

incIndex
  :: Block a
  -> Natural
incIndex =
  (^. blockIndex . _Wrapped . to (+1) )

newtype BlockChain a = BC (Seq (Block a))
  deriving (Show)
makeWrapped ''BlockChain

newtype New a = New a
newtype Old a = Old a
makeWrapped ''New
makeWrapped ''Old

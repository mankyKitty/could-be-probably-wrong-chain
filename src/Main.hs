{-# LANGUAGE RecursiveDo  #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import           Control.Applicative         (pure, (<$), (<$>), (<*>))
import           Control.Category            ((.))
import           Control.Lens                (failing, over, re, to, view, (%~),
                                              (^.), (^?), _Just, _Left,
                                              _Nothing, _Right, _Wrapped, _head)
import           Control.Monad               ((>=>), (>>=))

import           Control.Error.Util          (note)

import           Data.Either                 (Either (Left, Right))
import           Data.Maybe                  (maybe)

import           Data.Foldable               (foldlM)
import           Data.Function               (($), (&))
import           Data.Functor                (fmap)
import           Data.Sequence               (ViewL ((:<)))
import qualified Data.Sequence               as S

import           Data.Text.Lens              (packed)

import           Reactive.Banana.Combinators ((<@>))
import qualified Reactive.Banana.Combinators as RB
import           Reactive.Banana.Frameworks  (MomentIO)
import qualified Reactive.Banana.Frameworks  as RB

import           Prelude                     ((+), (/=), (<))

import           System.IO                   (IO, putStrLn)

import           Hash                        (AsHashable, calculateHash,
                                              calculateHashForBlock)
import           Types                       (Block (Block), BlockChain,
                                              Error (..), New (New), Old (Old),
                                              Timestamp, blockHash, blockIndex,
                                              blockPreviousHash, incIndex)

generateNextBlock
  :: AsHashable a
  => Block a
  -> Timestamp
  -> a
  -> Block a
generateNextBlock prevBlk nextTS a =
  let nextIx = incIndex prevBlk ^. re _Wrapped
  in
    Block
      nextIx
      (prevBlk ^. blockHash . _Wrapped . re _Wrapped)
      nextTS
      a
      ( calculateHash nextIx ( prevBlk ^. blockHash ) nextTS a )

createGenesisBlock
  :: AsHashable a
  => a
  -> Timestamp
  -> Block a
createGenesisBlock a ts =
  let
    pHash = "0" ^. packed . re _Wrapped
    ix = 0 ^. re _Wrapped
  in
    Block ix pHash ts a
    ( calculateHash
        ix
        ( pHash ^. _Wrapped . re _Wrapped )
        ts
        a
    )

validateNewBlock
  :: AsHashable a
  => Old ( Block a )
  -> New ( Block a )
  -> Either Error (Block a)
validateNewBlock ( Old preB ) ( New newB ) =
  let
    okay chk e v =
      if chk then Left e else Right v

    indexOkay = okay
      ( incIndex preB /= newB ^. blockIndex . _Wrapped )
      InvalidIndex

    prevHashOkay = okay
      ( preB ^. blockHash . _Wrapped /= newB ^. blockPreviousHash . _Wrapped )
      InvalidPreviousHash

    currHashOkay = okay
      ( calculateHashForBlock newB /= newB ^. blockHash )
      InvalidCurrentHash
  in
    ( indexOkay >=> prevHashOkay >=> currHashOkay ) newB

replaceChain
  :: AsHashable a
  => Old ( BlockChain a )
  -> New ( BlockChain a )
  -> Either Error (BlockChain a)
replaceChain ( Old oldBlockC ) ( New newBlockC ) =
  let
    bcLen = view (_Wrapped . to S.length)

    lenOkay v =
      if bcLen oldBlockC < bcLen v
      then Right v
      else Left OldChainLongerThanNew

    wrapperFlip b b' =
      Old <$> validateNewBlock b ( New b' )

    chainValid v =
      case v ^. _Wrapped . to S.viewl of
        S.EmptyL -> Left NewChainNotValid
        (h :< t) -> v <$ foldlM wrapperFlip (Old h) t
   in
    (lenOkay >=> chainValid) newBlockC

data EventHandler a = EventHandler
  { addHandler  :: RB.AddHandler a
  , fireHandler :: a -> IO ()
  }

bcNetwork
  :: AsHashable a
  => Timestamp
  -> EventHandler a
  -> EventHandler Timestamp
  -> BlockChain a
  -> MomentIO ()
bcNetwork startTime hBlock hTimestamp initialChain = mdo
  let
    acquireH =
      RB.fromAddHandler . addHandler

    extendChain
      :: AsHashable a
      => BlockChain a
      -> (Timestamp, a)
      -> Either Error (BlockChain a)
    extendChain bc (newTs, newA) = do
      currH <- note EmptyBlockChain $ bc ^? _Wrapped . _head
      newB <- validateNewBlock
        (Old currH)
        (New $ generateNextBlock currH newTs newA)
      replaceChain
        (Old bc)
        (New $ bc & _Wrapped %~ (S.|> newB))

  bTimestamp <- acquireH hTimestamp >>= RB.stepper startTime
  eDataInput <- acquireH hBlock

  let
    eTagged             = pure (,) <*> bTimestamp <@> eDataInput
    (eError, eNewChain) = RB.split (extendChain <$> bChain <@> eTagged)

  bChain <- RB.stepper initialChain eNewChain

  _f bChain

main :: IO ()
main = putStrLn "Hello, Haskell!"

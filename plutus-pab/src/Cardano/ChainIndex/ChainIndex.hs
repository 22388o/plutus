{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Cardano.ChainIndex.ChainIndex
    ( processIndexEffectsOld
    , processIndexEffects
    , startWatching
    , watchedAddresses
    , confirmedBlocks
    , syncStateOld
    , syncState
    , healthcheck
    , datumFromHash
    , validatorFromHash
    , mintingPolicyFromHash
    , stakeValidatorFromHash
    , txOutFromRef
    , txFromTxId
    , utxoSetMembership
    , utxoSetAtPubKeyAddress
    , utxoSetAtScriptAddress
    , getTip
    ) where

import           Cardano.BM.Data.Trace            (Trace)
import           Control.Concurrent.MVar          (MVar, putMVar, takeMVar)
import           Control.Monad.Freer              hiding (run)
import           Control.Monad.Freer.Error
import qualified Control.Monad.Freer.State        as Eff
import           Control.Monad.IO.Class           (MonadIO (..))
import           Data.Foldable                    (traverse_)
import           Data.Function                    ((&))
import           Ledger.Blockchain                (Block)
import           Ledger.Slot                      (Slot)
import           Servant                          (NoContent (NoContent))

import           Cardano.ChainIndex.Types
import           Data.Hashable                    (hash)
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding               as Text
import           Ledger                           (ChainIndexTxOut, Datum, DatumHash (..), MintingPolicy,
                                                   MintingPolicyHash (..), PubKeyHash (..), StakeValidator,
                                                   StakeValidatorHash (..), TxId (..), TxOutRef (..), Validator,
                                                   ValidatorHash (..), eitherTx, txId)
import           Ledger.Address                   (Address)
import           Ledger.AddressMap                (AddressMap)
import           Ledger.Credential                (Credential (PubKeyCredential, ScriptCredential))
import qualified Plutus.ChainIndex                as ChainIndex
import           Plutus.PAB.Monitoring.Monitoring (convertLog, handleLogMsgTrace)
import qualified PlutusTx.Builtins                as Builtins
import           Wallet.Effects                   (ChainIndexEffect)
import qualified Wallet.Effects                   as ChainIndexOld
import           Wallet.Emulator.ChainIndex       (ChainIndexControlEffect, ChainIndexEvent)
import qualified Wallet.Emulator.ChainIndex       as ChainIndexOld
import           Wallet.Emulator.NodeClient       (ChainClientNotification (BlockValidated, SlotChanged))

healthcheck :: Monad m => m NoContent
healthcheck = pure NoContent

-- TODO: Remove. Old chain index
startWatching :: (Member ChainIndexEffect effs) => Address -> Eff effs NoContent
startWatching addr = ChainIndexOld.startWatching addr >> pure NoContent

-- TODO: Remove. Old chain index
watchedAddresses :: (Member ChainIndexEffect effs) => Eff effs AddressMap
watchedAddresses = ChainIndexOld.watchedAddresses

-- TODO: Remove. Old chain index
confirmedBlocks :: (Member ChainIndexEffect effs) => Eff effs [Block]
confirmedBlocks = ChainIndexOld.confirmedBlocks

datumFromHash :: (Member ChainIndex.ChainIndexQueryEffect effs) => Text -> Eff effs (Maybe Datum)
datumFromHash = ChainIndex.datumFromHash . DatumHash . Builtins.toBuiltin . Text.encodeUtf8

validatorFromHash :: (Member ChainIndex.ChainIndexQueryEffect effs) => Text -> Eff effs (Maybe Validator)
validatorFromHash = ChainIndex.validatorFromHash . ValidatorHash . Builtins.toBuiltin . Text.encodeUtf8

mintingPolicyFromHash :: (Member ChainIndex.ChainIndexQueryEffect effs) => Text -> Eff effs (Maybe MintingPolicy)
mintingPolicyFromHash = ChainIndex.mintingPolicyFromHash . MintingPolicyHash . Builtins.toBuiltin . Text.encodeUtf8

stakeValidatorFromHash :: (Member ChainIndex.ChainIndexQueryEffect effs) => Text -> Eff effs (Maybe StakeValidator)
stakeValidatorFromHash = ChainIndex.stakeValidatorFromHash . StakeValidatorHash . Builtins.toBuiltin . Text.encodeUtf8

txOutFromRef :: (Member ChainIndex.ChainIndexQueryEffect effs) => Text -> Integer -> Eff effs (Maybe ChainIndexTxOut)
txOutFromRef txid idx =
    ChainIndex.txOutFromRef $ TxOutRef (TxId $ Builtins.toBuiltin $ Text.encodeUtf8 txid) idx

txFromTxId :: (Member ChainIndex.ChainIndexQueryEffect effs) => Text -> Eff effs (Maybe ChainIndex.ChainIndexTx)
txFromTxId = ChainIndex.txFromTxId . TxId . Builtins.toBuiltin . Text.encodeUtf8

utxoSetMembership ::
    (Member ChainIndex.ChainIndexQueryEffect effs)
    => Text
    -> Integer
    -> Eff effs (ChainIndex.Tip, Bool)
utxoSetMembership txid idx =
    ChainIndex.utxoSetMembership $ TxOutRef (TxId $ Builtins.toBuiltin $ Text.encodeUtf8 txid) idx

utxoSetAtPubKeyAddress ::
    (Member ChainIndex.ChainIndexQueryEffect effs)
    => Text
    -> Eff effs (ChainIndex.Tip, ChainIndex.Page TxOutRef)
utxoSetAtPubKeyAddress =
    ChainIndex.utxoSetAtAddress . PubKeyCredential . PubKeyHash . Builtins.toBuiltin . Text.encodeUtf8

utxoSetAtScriptAddress ::
    (Member ChainIndex.ChainIndexQueryEffect effs)
    => Text
    -> Eff effs (ChainIndex.Tip, ChainIndex.Page TxOutRef)
utxoSetAtScriptAddress =
    ChainIndex.utxoSetAtAddress . ScriptCredential . ValidatorHash . Builtins.toBuiltin . Text.encodeUtf8

getTip ::
    (Member ChainIndex.ChainIndexQueryEffect effs)
    => Eff effs ChainIndex.Tip
getTip = ChainIndex.getTip

-- | Update the chain index by asking the node for new blocks since the last
--   time.
--
-- TODO: Remove. Old chain index.
syncStateOld ::
    ( Member ChainIndexControlEffect effs
    )
    => Block
    -> Slot
    -> Eff effs ()
syncStateOld block slot = do
    traverse_ ChainIndexOld.chainIndexNotify [BlockValidated block, SlotChanged slot]

-- | Update the chain index by asking the node for new blocks since the last
--   time.
syncState ::
    ( Member ChainIndex.ChainIndexControlEffect effs
    , Member ChainIndex.ChainIndexQueryEffect effs
    )
    => Block
    -> Slot
    -> Eff effs ()
syncState block slot = do
    currentTip <- ChainIndex.getTip
    let nextBlockNo = case currentTip of ChainIndex.TipAtGenesis -> 0
                                         ChainIndex.Tip _ _ n    -> n + 1
    let blockId = ChainIndex.BlockId
                $ (Text.encodeUtf8 . Text.pack . show . hash)
                $ foldMap (getTxId . eitherTx txId txId) block
    let newTip = ChainIndex.Tip slot blockId nextBlockNo
    ChainIndex.appendBlock newTip (fmap ChainIndex.fromOnChainTx block)

-- TODO Remove. Uses old chain index
processIndexEffectsOld ::
    MonadIO m
    => ChainIndexTrace
    -> MVar AppStateOld
    -> Eff (ChainIndexEffectsOld IO) a
    -> m a
processIndexEffectsOld trace stateVar eff = do
    AppStateOld {_indexStateOld, _indexEventsOld} <- liftIO $ takeMVar stateVar
    (result, newState) <- liftIO
        $ ChainIndexOld.handleChainIndexControl eff
        & ChainIndexOld.handleChainIndex
        & Eff.runState _indexStateOld
        & interpret (handleLogMsgTrace (toChainIndexServerMsg trace))
        & runM
    liftIO $ putMVar stateVar AppStateOld{_indexStateOld=newState, _indexEventsOld=_indexEventsOld}
    pure result
        where
            toChainIndexServerMsg :: Trace m ChainIndexServerMsg -> Trace m ChainIndexEvent
            toChainIndexServerMsg = convertLog ChainEventOld

processIndexEffects ::
    MonadIO m
    => ChainIndexTrace
    -> MVar AppState
    -> Eff (ChainIndexEffects IO) a
    -> m a
processIndexEffects trace stateVar eff = do
    AppState {_indexState, _indexEvents} <- liftIO $ takeMVar stateVar
    resultE <- liftIO
        $ interpret ChainIndex.handleControl eff
        & interpret ChainIndex.handleQuery
        & Eff.runState _indexState
        & interpret (handleLogMsgTrace (toChainIndexServerMsg trace))
        & runError
        & runM
    case resultE of
      Left e -> error (show e)
      Right (result, newState) -> do
        liftIO $ putMVar stateVar AppState{_indexState=newState, _indexEvents=_indexEvents}
        pure result
  where
      toChainIndexServerMsg :: Trace m ChainIndexServerMsg -> Trace m ChainIndex.ChainIndexLog
      toChainIndexServerMsg = convertLog ChainEvent

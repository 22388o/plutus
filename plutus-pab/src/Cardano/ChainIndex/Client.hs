{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Cardano.ChainIndex.Client where

import           Cardano.ChainIndex.API     (API, APIOld)
import           Control.Monad              (void)
import           Control.Monad.Freer
import           Control.Monad.Freer.Error  (Error, throwError)
import           Control.Monad.Freer.Reader (Reader, ask)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Data.Proxy                 (Proxy (Proxy))
import           Ledger                     (Address, ChainIndexTxOut, Datum, DatumHash (..), MintingPolicy,
                                             MintingPolicyHash (..), PubKeyHash (..), StakeValidator,
                                             StakeValidatorHash (..), TxId (..), TxOutRef (..), Validator,
                                             ValidatorHash (..))
import           Ledger.AddressMap          (AddressMap)
import           Ledger.Blockchain          (Block)
import           Ledger.Credential          (Credential (..))
import           Servant                    (NoContent, (:<|>) (..))
import           Servant.Client             (ClientEnv, ClientError, ClientM, client, runClientM)

import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as Text
import           Plutus.ChainIndex          (ChainIndexQueryEffect (..), ChainIndexTx, Page, Tip)
import qualified PlutusTx.Builtins          as Builtins
import           Wallet.Effects             (AddressChangeRequest, AddressChangeResponse, ChainIndexEffect (..))

healthCheckOld :: ClientM NoContent
startWatching :: Address -> ClientM NoContent
watchedAddresses :: ClientM AddressMap
confirmedBlocks :: ClientM [Block]
addressChanged :: AddressChangeRequest -> ClientM AddressChangeResponse
(healthCheckOld, startWatching, watchedAddresses, confirmedBlocks, addressChanged) =
  (healthCheck_, startWatching_, watchedAddresses_, confirmedBlocks_, addressChanged_)
  where
    healthCheck_ :<|> startWatching_ :<|> watchedAddresses_ :<|> confirmedBlocks_ :<|> addressChanged_ =
        client (Proxy @APIOld)

handleChainIndexClient ::
  forall m effs.
  ( LastMember m effs
  , Member (Reader ClientEnv) effs
  , MonadIO m
  , Member (Error ClientError) effs)
  => ChainIndexEffect
  ~> Eff effs
handleChainIndexClient event = do
    clientEnv <- ask
    let
        runClient :: forall a. ClientM a -> Eff effs a
        runClient a = (sendM $ liftIO $ runClientM a clientEnv) >>= either throwError pure
    case event of
        StartWatching a    -> void (runClient (startWatching a))
        WatchedAddresses   -> runClient watchedAddresses
        ConfirmedBlocks    -> runClient confirmedBlocks
        AddressChanged req -> runClient (addressChanged req)

healthCheck :: ClientM NoContent
datumFromHash :: Text -> ClientM (Maybe Datum)
validatorFromHash :: Text -> ClientM (Maybe Validator)
mintingPolicyFromHash :: Text -> ClientM (Maybe MintingPolicy)
stakeValidatorFromHash :: Text -> ClientM (Maybe StakeValidator)
txOutFromRef :: Text -> Integer -> ClientM (Maybe ChainIndexTxOut)
txFromTxId :: Text -> ClientM (Maybe ChainIndexTx)
utxoSetMembership :: Text -> Integer -> ClientM (Tip, Bool)
utxoSetAtPubKeyAddress :: Text -> ClientM (Tip, Page TxOutRef)
utxoSetAtScriptAddress :: Text -> ClientM (Tip, Page TxOutRef)
getTip :: ClientM Tip
(   healthCheck
  , datumFromHash
  , validatorFromHash
  , mintingPolicyFromHash
  , stakeValidatorFromHash
  , txOutFromRef
  , txFromTxId
  , utxoSetMembership
  , utxoSetAtPubKeyAddress
  , utxoSetAtScriptAddress
  , getTip) =

  ( healthCheck_
  , datumFromHash_
  , validatorFromHash_
  , mintingPolicyFromHash_
  , stakeValidatorFromHash_
  , txOutFromRef_
  , txFromTxId_
  , utxoSetMembership_
  , utxoSetAtPubKeyAddress_
  , utxoSetAtScriptAddress_
  , getTip_
  )
  where
    healthCheck_
      :<|> datumFromHash_
      :<|> validatorFromHash_
      :<|> mintingPolicyFromHash_
      :<|> stakeValidatorFromHash_
      :<|> txOutFromRef_
      :<|> txFromTxId_
      :<|> utxoSetMembership_
      :<|> utxoSetAtPubKeyAddress_
      :<|> utxoSetAtScriptAddress_
      :<|> getTip_ =
        client (Proxy @API)

handleChainIndexClient' ::
  forall m effs.
  ( LastMember m effs
  , Member (Reader ClientEnv) effs
  , MonadIO m
  , Member (Error ClientError) effs)
  => ChainIndexQueryEffect
  ~> Eff effs
handleChainIndexClient' event = do
    clientEnv <- ask
    let
        runClient :: forall a. ClientM a -> Eff effs a
        runClient a = (sendM $ liftIO $ runClientM a clientEnv) >>= either throwError pure
    case event of
      DatumFromHash (DatumHash h)         ->
          runClient $ datumFromHash $ Text.decodeUtf8 $ Builtins.fromBuiltin  h
      ValidatorFromHash (ValidatorHash h) ->
          runClient $ validatorFromHash $ Text.decodeUtf8 $ Builtins.fromBuiltin  h
      MintingPolicyFromHash (MintingPolicyHash h) ->
          runClient $ mintingPolicyFromHash $ Text.decodeUtf8 $ Builtins.fromBuiltin  h
      StakeValidatorFromHash (StakeValidatorHash h) ->
          runClient $ stakeValidatorFromHash $ Text.decodeUtf8 $ Builtins.fromBuiltin  h
      TxOutFromRef TxOutRef {txOutRefId, txOutRefIdx}          ->
          runClient $ txOutFromRef (Text.decodeUtf8 $ Builtins.fromBuiltin  $ getTxId txOutRefId) txOutRefIdx
      TxFromTxId (TxId txid)            ->
          runClient $ txFromTxId $ Text.decodeUtf8 $ Builtins.fromBuiltin  txid
      UtxoSetMembership TxOutRef {txOutRefId, txOutRefIdx} ->
          runClient $ utxoSetMembership (Text.decodeUtf8 $ Builtins.fromBuiltin  $ getTxId txOutRefId) txOutRefIdx
      UtxoSetAtAddress (PubKeyCredential (PubKeyHash h))      ->
          runClient $ utxoSetAtPubKeyAddress $ Text.decodeUtf8 $ Builtins.fromBuiltin h
      UtxoSetAtAddress (ScriptCredential (ValidatorHash h))      ->
          runClient $ utxoSetAtScriptAddress $ Text.decodeUtf8 $ Builtins.fromBuiltin h
      GetTip                  -> runClient getTip

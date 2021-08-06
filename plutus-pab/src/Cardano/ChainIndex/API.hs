{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.ChainIndex.API where

import           Data.Text         (Text)
import           Ledger            (Address, ChainIndexTxOut, Datum, MintingPolicy, StakeValidator, TxOutRef, Validator)
import           Ledger.AddressMap (AddressMap)
import           Ledger.Blockchain (Block)
import           Plutus.ChainIndex (ChainIndexTx, Page, Tip)
import           Servant.API       (Capture, Get, JSON, NoContent, Post, ReqBody, (:<|>), (:>))
import           Wallet.Effects    (AddressChangeRequest, AddressChangeResponse)

type APIOld
     = "healthcheck" :> Get '[JSON] NoContent
     :<|> "start-watching" :> ReqBody '[JSON] Address :> Post '[JSON] NoContent
     :<|> "watched-addresses" :> Get '[JSON] AddressMap
     :<|> "confirmed-blocks" :> Get '[JSON] [Block]
     :<|> "next-tx" :> ReqBody '[JSON] AddressChangeRequest :> Post '[JSON] AddressChangeResponse

type API
     = "healthcheck" :> Get '[JSON] NoContent
     :<|> "datum" :> Capture "datumhash" Text :> Get '[JSON] (Maybe Datum)
     :<|> "validator" :> Capture "validatorhash" Text
                      :> Get '[JSON] (Maybe Validator)
     :<|> "minting-policy" :> Capture "mintingpolicyhash" Text
                           :> Get '[JSON] (Maybe MintingPolicy)
     :<|> "stake-validator" :> Capture "stakevalidatorhash" Text
                           :> Get '[JSON] (Maybe StakeValidator)
     :<|> "tx-out" :> Capture "txid" Text
                   :> Capture "idx" Integer
                   :> Get '[JSON] (Maybe ChainIndexTxOut)
     :<|> "tx" :> Capture "txid" Text
               :> Get '[JSON] (Maybe ChainIndexTx)
     :<|> "utxo-set-membership" :> Capture "txid" Text
                                :> Capture "idx" Integer
                                :> Get '[JSON] (Tip, Bool)
     :<|> "utxos-at-public-key-address" :> Capture "pubkeyhash" Text
                                        :> Get '[JSON] (Tip, Page TxOutRef)
     :<|> "utxos-at-script-address" :> Capture "validatorhash" Text
                                    :> Get '[JSON] (Tip, Page TxOutRef)
     :<|> "tip" :> Get '[JSON] Tip

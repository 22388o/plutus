{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}

module Cardano.ChainIndex.Server(
    -- $chainIndex
    mainOld
    , main
    , ChainIndexConfig(..)
    , ChainIndexServerMsg
    , syncStateOld
    , syncState
    ) where

import           Control.Concurrent.MVar             (MVar, newMVar)
import           Control.Monad.Freer.Extras.Log
import           Servant.Client                      (BaseUrl (baseUrlPort))

import           Data.Coerce                         (coerce)
import           Plutus.PAB.Monitoring.Util          (runLogEffects)
import qualified Wallet.Effects                      as WalletEffects

import           Cardano.ChainIndex.ChainIndex       (confirmedBlocks, datumFromHash, getTip, healthcheck,
                                                      mintingPolicyFromHash, processIndexEffects,
                                                      processIndexEffectsOld, stakeValidatorFromHash, startWatching,
                                                      syncState, syncStateOld, txFromTxId, txOutFromRef,
                                                      utxoSetAtPubKeyAddress, utxoSetAtScriptAddress, utxoSetMembership,
                                                      validatorFromHash, watchedAddresses)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Function                       ((&))
import           Data.Proxy                          (Proxy (Proxy))
import           Ledger.Blockchain                   (Block)
import           Ledger.TimeSlot                     (SlotConfig)
import qualified Network.Wai.Handler.Warp            as Warp
import           Servant                             (Application, hoistServer, serve, (:<|>) ((:<|>)))

import           Cardano.ChainIndex.API
import           Cardano.ChainIndex.Types
import           Cardano.Protocol.Socket.Mock.Client (runChainSync)
import           Control.Concurrent.Availability     (Availability, available)
import           Ledger.Slot                         (Slot (..))

-- $chainIndex
-- The PAB chain index that keeps track of transaction data (UTXO set enriched
-- with datums)

appOld :: ChainIndexTrace -> MVar AppStateOld -> Application
appOld trace stateVar =
    serve (Proxy @APIOld) $
    hoistServer
        (Proxy @APIOld)
        (\x -> processIndexEffectsOld trace stateVar x)
        (    healthcheck
        :<|> startWatching
        :<|> watchedAddresses
        :<|> confirmedBlocks
        :<|> WalletEffects.addressChanged
        )

app :: ChainIndexTrace -> MVar AppState -> Application
app trace stateVar =
    serve (Proxy @API) $
    hoistServer
        (Proxy @API)
        (liftIO . processIndexEffects trace stateVar)
        (    healthcheck
        :<|> datumFromHash
        :<|> validatorFromHash
        :<|> mintingPolicyFromHash
        :<|> stakeValidatorFromHash
        :<|> txOutFromRef
        :<|> txFromTxId
        :<|> utxoSetMembership
        :<|> utxoSetAtPubKeyAddress
        :<|> utxoSetAtScriptAddress
        :<|> getTip
        )

mainOld :: ChainIndexTrace -> ChainIndexConfig -> FilePath -> SlotConfig -> Availability -> IO ()
mainOld trace ChainIndexConfig{ciBaseUrl} socketPath slotConfig availability = runLogEffects trace $ do
    mVarState <- liftIO $ newMVar initialAppStateOld

    logInfo StartingNodeClientThread
    _ <- liftIO $ runChainSync socketPath slotConfig $ updateChainState mVarState

    logInfo $ StartingChainIndex servicePort
    liftIO $ Warp.runSettings warpSettings $ appOld trace mVarState
        where
            isAvailable = available availability
            servicePort = baseUrlPort (coerce ciBaseUrl)
            warpSettings = Warp.defaultSettings & Warp.setPort servicePort & Warp.setBeforeMainLoop isAvailable
            updateChainState :: MVar AppStateOld -> Block -> Slot -> IO ()
            updateChainState mv block slot = do
              processIndexEffectsOld trace mv $ syncStateOld block slot

main :: ChainIndexTrace -> ChainIndexConfig -> FilePath -> SlotConfig -> Availability -> IO ()
main trace ChainIndexConfig{ciBaseUrl} socketPath slotConfig availability = runLogEffects trace $ do
    mVarState <- liftIO $ newMVar initialAppState

    logInfo StartingNodeClientThread
    _ <- liftIO $ runChainSync socketPath slotConfig $ updateChainState mVarState

    logInfo $ StartingChainIndex servicePort
    liftIO $ Warp.runSettings warpSettings $ app trace mVarState
        where
            isAvailable = available availability
            servicePort = baseUrlPort (coerce ciBaseUrl)
            warpSettings = Warp.defaultSettings & Warp.setPort servicePort & Warp.setBeforeMainLoop isAvailable
            updateChainState :: MVar AppState -> Block -> Slot -> IO ()
            updateChainState mv block slot =
              processIndexEffects trace mv $ syncState block slot

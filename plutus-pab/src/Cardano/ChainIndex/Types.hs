{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Cardano.ChainIndex.Types where

import           Control.Lens                   (makeLenses)
import           Control.Monad.Freer.Error
import           Control.Monad.Freer.Extras.Log (LogMessage)
import           Control.Monad.Freer.State
import           Data.Aeson                     (FromJSON, ToJSON)
import           Data.Default                   (Default, def)
import           Data.Sequence                  (Seq)
import           Data.Text.Prettyprint.Doc      (Pretty (..), parens, (<+>))
import           GHC.Generics                   (Generic)
import           Servant.Client                 (BaseUrl (..), Scheme (..))

import           Cardano.BM.Data.Trace          (Trace)
import           Cardano.BM.Data.Tracer         (ToObject (..))
import           Cardano.BM.Data.Tracer.Extras  (Tagged (..), mkObjectStr)
import           Control.Monad.Freer.Extras     (LogMsg)
import           Ledger.Address                 (Address)
import qualified Plutus.ChainIndex              as ChainIndex
import           Wallet.Effects                 (ChainIndexEffect)
import           Wallet.Emulator.ChainIndex     (ChainIndexControlEffect, ChainIndexEvent, ChainIndexState)

-- TODO: Remove. Old chain index
type ChainIndexEffectsOld m
     = '[ ChainIndexControlEffect
        , ChainIndexEffect
        , State ChainIndexState
        , LogMsg ChainIndexEvent
        , m
        ]

type ChainIndexEffects m
     = '[ ChainIndex.ChainIndexControlEffect
        , ChainIndex.ChainIndexQueryEffect
        , State ChainIndex.ChainIndexEmulatorState
        , LogMsg ChainIndex.ChainIndexLog
        , Error ChainIndex.ChainIndexError
        , m
        ]

newtype ChainIndexUrl = ChainIndexUrl BaseUrl
    deriving (Eq, Show, FromJSON, ToJSON) via BaseUrl

-- TODO: Remove. Old chain index.
data AppStateOld =
    AppStateOld
        { _indexStateOld  :: ChainIndexState
        , _indexEventsOld :: Seq (LogMessage ChainIndexEvent)
        } deriving (Eq, Show)

data AppState =
    AppState
        { _indexState  :: ChainIndex.ChainIndexEmulatorState
        , _indexEvents :: Seq (LogMessage ChainIndex.ChainIndexLog)
        } deriving (Eq, Show)

initialAppStateOld :: AppStateOld
initialAppStateOld = AppStateOld mempty mempty

initialAppState :: AppState
initialAppState = AppState mempty mempty

data ChainIndexConfig =
    ChainIndexConfig
        { ciBaseUrl          :: ChainIndexUrl
        , ciWatchedAddresses :: [Address] -- TODO: Remove. Used by old chain index
        }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

defaultChainIndexConfig :: ChainIndexConfig
defaultChainIndexConfig =
  ChainIndexConfig
    -- See Note [pab-ports] in "test/full/Plutus/PAB/CliSpec.hs".
    { ciBaseUrl = ChainIndexUrl $ BaseUrl Http "localhost" 9083 ""
    , ciWatchedAddresses = []
    }

instance Default ChainIndexConfig where
  def = defaultChainIndexConfig

makeLenses ''AppStateOld
makeLenses ''AppState
makeLenses ''ChainIndexConfig

-- | Messages from the ChainIndex Server
data ChainIndexServerMsg =
    -- | Starting a node client thread
      StartingNodeClientThread
    -- | Starting ChainIndex service
    | StartingChainIndex
        Int    -- ^ Port number
      -- | Received transaction
    | ReceivedBlocksTxns
        Int    -- ^ Blocks
        Int    -- ^ Transactions
    | ChainEventOld ChainIndexEvent -- TODO: Remove. Used by old chain index
    | ChainEvent ChainIndex.ChainIndexLog
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

type ChainIndexTrace = Trace IO ChainIndexServerMsg

instance Pretty ChainIndexServerMsg where
    pretty = \case
        ReceivedBlocksTxns blocks txns -> "Received" <+> pretty blocks <+> "blocks" <+> parens (pretty txns <+> "transactions")
        StartingNodeClientThread -> "Starting node client thread"
        StartingChainIndex port -> "Starting chain index on port: " <> pretty port
        ChainEventOld e -> "Processing chain index event: " <> pretty e
        ChainEvent e -> "Processing chain index event: " <> pretty e

instance ToObject ChainIndexServerMsg where
    toObject _ = \case
      ReceivedBlocksTxns x y   -> mkObjectStr "received block transactions" (Tagged @"blocks" x, Tagged @"transactions" y)
      StartingNodeClientThread -> mkObjectStr "starting node client thread" ()
      StartingChainIndex p     -> mkObjectStr "starting chain index" (Tagged @"port" p)
      ChainEventOld e             -> mkObjectStr "processing chain event" (Tagged @"event" e)
      ChainEvent e             -> mkObjectStr "processing chain event" (Tagged @"event" e)

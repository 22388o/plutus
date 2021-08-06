{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeApplications   #-}
module Plutus.Contract.Tx where

import           Control.Lens
import           Data.Maybe                       (fromMaybe)

import           Ledger                           (Redeemer (..), TxOutRef, TxOutTx, Validator)
import qualified Ledger.Address                   as Address
import           Ledger.AddressMap                (AddressMap)
import           Ledger.Constraints.TxConstraints (UntypedConstraints)
import qualified Plutus.Contract.Typed.Tx         as Typed
import qualified PlutusTx

-- | A set of constraints for a transaction that collects script outputs
--   from the address of the given validator script, using the same redeemer
--   script for all outputs.
collectFromScriptOld
    :: AddressMap
    -> Validator
    -> Redeemer
    -> UntypedConstraints
collectFromScriptOld = collectFromScriptFilterOld (\_ -> const True)

-- | See
collectFromScriptFilterOld
    :: (TxOutRef -> TxOutTx -> Bool)
    -> AddressMap
    -> Validator
    -> Redeemer
    -> UntypedConstraints
collectFromScriptFilterOld flt am vls (Redeemer red) =
    let mp'  = fromMaybe mempty $ am ^. at (Address.scriptAddress vls)
    in Typed.collectFromScriptFilterOld @PlutusTx.BuiltinData @PlutusTx.BuiltinData flt mp' red

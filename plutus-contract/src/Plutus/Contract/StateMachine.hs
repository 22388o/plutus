{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
module Plutus.Contract.StateMachine(
    -- $statemachine
    StateMachineClient(..)
    , StateMachineClientOld(..)
    , TxConstraints
    , SMContractError(..)
    , AsSMContractError(..)
    , SM.StateMachine(..)
    , SM.StateMachineInstance(..)
    , SM.State(..)
    , OnChainStateOld(..)
    , OnChainState(..)
    , WaitingResultOld(..)
    , WaitingResult(..)
    , InvalidTransition(..)
    , TransitionResult(..)
    , ThreadToken(..)
    -- * Constructing the machine instance
    , SM.mkValidator
    , SM.mkStateMachine
    -- * Constructing the state machine client
    , mkStateMachineClientOld
    , mkStateMachineClient
    , defaultChooserOld
    , defaultChooser
    , getStatesOld
    , getStates
    -- * Running the state machine
    , runGuardedStepOld
    , runGuardedStep
    , runStepOld
    , runStep
    , runInitialiseOld
    , runInitialise
    , runGuardedStepWithOld
    , runGuardedStepWith
    , runStepWithOld
    , runStepWith
    , runInitialiseWithOld
    , runInitialiseWith
    , getThreadTokenOld
    , getThreadToken
    , getOnChainStateOld
    , getOnChainState
    , waitForUpdateOld
    , waitForUpdate
    , waitForUpdateUntilSlotOld
    , waitForUpdateUntilSlot
    , waitForUpdateUntilTimeOld
    , waitForUpdateUntilTime
    , waitForUpdateTimeoutOld
    , waitForUpdateTimeout
    -- * Lower-level API
    , StateMachineTransition(..)
    , mkStepOld
    , mkStep
    -- * Re-exports
    , Void
    ) where

import           Control.Lens
import           Control.Monad.Error.Lens
import           Data.Aeson                                   (FromJSON, ToJSON)
import           Data.Default                                 (Default (def))
import           Data.Either                                  (rights)
import           Data.List.NonEmpty                           (toList)
import           Data.Map                                     (Map)
import qualified Data.Map                                     as Map
import           Data.Maybe                                   (catMaybes, listToMaybe, mapMaybe)
import qualified Data.Set                                     as Set
import           Data.Text                                    (Text)
import qualified Data.Text                                    as Text
import           Data.Void                                    (Void, absurd)
import           GHC.Generics                                 (Generic)
import           Ledger                                       (POSIXTime, Slot, TxOutRef, Value, scriptCurrencySymbol)
import qualified Ledger
import           Ledger.Constraints                           (ScriptLookups, TxConstraints (..), mintingPolicy,
                                                               mustMintValueWithRedeemer, mustPayToTheScript,
                                                               mustSpendPubKeyOutput, mustSpendPubKeyOutputOld)
import           Ledger.Constraints.OffChain                  (UnbalancedTx)
import qualified Ledger.Constraints.OffChain                  as Constraints
import           Ledger.Constraints.TxConstraints             (InputConstraint (..), OutputConstraint (..))
import           Ledger.Crypto                                (pubKeyHash)
import qualified Ledger.TimeSlot                              as TimeSlot
import qualified Ledger.Tx                                    as Tx
import qualified Ledger.Typed.Scripts                         as Scripts
import           Ledger.Typed.Tx                              (TypedScriptTxOut (..))
import qualified Ledger.Typed.Tx                              as Typed
import qualified Ledger.Value                                 as Value
import           Plutus.ChainIndex                            (ChainIndexTx (..))
import           Plutus.Contract
import           Plutus.Contract.StateMachine.MintingPolarity (MintingPolarity (..))
import           Plutus.Contract.StateMachine.OnChain         (State (..), StateMachine (..), StateMachineInstance (..))
import qualified Plutus.Contract.StateMachine.OnChain         as SM
import           Plutus.Contract.StateMachine.ThreadToken     (ThreadToken (..), curPolicy, ttOutRef)
import           Plutus.Contract.Wallet                       (getUnspentOutput, getUnspentOutputOld)
import qualified PlutusTx
import           PlutusTx.Monoid                              (inv)

-- $statemachine
-- To write your contract as a state machine you need
-- * Two types @state@ and @input@ for the state and inputs of the machine
-- * A 'SM.StateMachineInstance state input' describing the transitions and
--   checks of the state machine (this is the on-chain code)
-- * A 'StateMachineClient state input' with the state machine instance and
--   an allocation function
--
-- In many cases it is enough to define the transition function
-- @t :: (state, Value) -> input -> Maybe (TxConstraints state)@ and use
-- 'mkStateMachine' and 'mkStateMachineClient' to get the client.
-- You can then use 'runInitialise' and 'runStep' to initialise and transition
-- the state machine. 'runStep' gets the current state from the utxo set and
-- makes the transition to the next state using the given input and taking care
-- of all payments.

-- | Typed representation of the on-chain state of a state machine instance
data OnChainStateOld s i =
    OnChainStateOld
        { ocsTxOut'    :: Typed.TypedScriptTxOut (SM.StateMachine s i) -- ^ Typed transaction output
        , ocsTxOutRef' :: Typed.TypedScriptTxOutRef (SM.StateMachine s i) -- ^ Typed UTXO
        , ocsTx'       :: Tx.Tx -- ^ Transaction that produced the output
        }

-- | Typed representation of the on-chain state of a state machine instance
data OnChainState s i =
    OnChainState
        { ocsTxOut    :: Typed.TypedScriptTxOut (SM.StateMachine s i) -- ^ Typed transaction output
        , ocsTxOutRef :: Typed.TypedScriptTxOutRef (SM.StateMachine s i) -- ^ Typed UTXO
        , ocsTx       :: ChainIndexTx -- ^ Transaction that produced the output
        }

-- TODO Remove uses old chain index
getInputOld ::
    forall i.
    (PlutusTx.FromData i)
    => TxOutRef
    -> Tx.Tx
    -> Maybe i
getInputOld outRef tx = do
    (_validator, Ledger.Redeemer r, _) <- listToMaybe $ mapMaybe Tx.inScripts $ filter (\Tx.TxIn{Tx.txInRef} -> outRef == txInRef) $ Set.toList $ Tx.txInputs tx
    PlutusTx.fromBuiltinData r

getInput ::
    forall i.
    (PlutusTx.FromData i)
    => TxOutRef
    -> ChainIndexTx
    -> Maybe i
getInput outRef tx = do
    (_validator, Ledger.Redeemer r, _) <- listToMaybe $ mapMaybe Tx.inScripts $ filter (\Tx.TxIn{Tx.txInRef} -> outRef == txInRef) $ Set.toList $ _citxInputs tx
    PlutusTx.fromBuiltinData r

getStatesOld
    :: forall s i
    . (PlutusTx.FromData s, PlutusTx.ToData s)
    => SM.StateMachineInstance s i
    -> Map Tx.TxOutRef Tx.TxOutTx
    -> [OnChainStateOld s i]
getStatesOld (SM.StateMachineInstance _ si) refMap =
    let lkp (ref, out) = do
            ocsTxOutRef' <- Typed.typeScriptTxOutRefOld (\r -> Map.lookup r refMap) si ref
            ocsTxOut' <- Typed.typeScriptTxOutOld si out
            pure OnChainStateOld{ocsTxOut', ocsTxOutRef', ocsTx' = Tx.txOutTxTx out}
    in rights $ fmap lkp $ Map.toList refMap

getStates
    :: forall s i
    . (PlutusTx.FromData s, PlutusTx.ToData s)
    => SM.StateMachineInstance s i
    -> Map Tx.TxOutRef (Tx.ChainIndexTxOut, ChainIndexTx)
    -> [OnChainState s i]
getStates (SM.StateMachineInstance _ si) refMap =
    let lkp (ref, (out, tx)) = do
            ocsTxOutRef <- Typed.typeScriptTxOutRef (\r -> fst <$> Map.lookup r refMap) si ref
            ocsTxOut <- Typed.typeScriptTxOut si ref out
            pure OnChainState{ocsTxOut, ocsTxOutRef, ocsTx = tx}
    in rights $ fmap lkp $ Map.toList refMap

-- | An invalid transition
data InvalidTransition s i =
    InvalidTransition
        { tfState :: Maybe (State s) -- ^ Current state. 'Nothing' indicates that there is no current state.
        , tfInput :: i -- ^ Transition that was attempted but failed
        }
        deriving stock (Eq, Show, Generic)
        deriving anyclass (ToJSON, FromJSON)

-- | Result of an attempted transition
data TransitionResult s i =
    TransitionFailure (InvalidTransition s i) -- ^ The transition is not allowed
    | TransitionSuccess s -- ^ The transition is allowed and results in a new state

data SMContractError =
    ChooserError Text
    | UnableToExtractTransition
    | SMCContractError ContractError
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''SMContractError

instance AsContractError SMContractError where
    _ContractError = _SMCContractError

-- | Client-side definition of a state machine.
--
-- TODO Delete, uses old chain index
data StateMachineClientOld s i = StateMachineClientOld
    { scInstance' :: SM.StateMachineInstance s i
    -- ^ The instance of the state machine, defining the machine's transitions,
    --   its final states and its check function.
    , scChooser'  :: [OnChainStateOld s i] -> Either SMContractError (OnChainStateOld s i)
    -- ^ A function that chooses the relevant on-chain state, given a list of
    --   all potential on-chain states found at the contract address.
    }

-- | Client-side definition of a state machine.
data StateMachineClient s i = StateMachineClient
    { scInstance :: SM.StateMachineInstance s i
    -- ^ The instance of the state machine, defining the machine's transitions,
    --   its final states and its check function.
    , scChooser  :: [OnChainState s i] -> Either SMContractError (OnChainState s i)
    -- ^ A function that chooses the relevant on-chain state, given a list of
    --   all potential on-chain states found at the contract address.
    }

-- | A state chooser function that fails if confronted with anything other
--   than exactly one output
--
-- TODO Remove uses old chain index
defaultChooserOld ::
    forall state input
    . [OnChainStateOld state input]
    -> Either SMContractError (OnChainStateOld state input)
defaultChooserOld [x] = Right x
defaultChooserOld xs  =
    let msg = "Found " <> show (length xs) <> " outputs, expected 1"
    in Left (ChooserError (Text.pack msg))

-- | A state chooser function that fails if confronted with anything other
--   than exactly one output
defaultChooser ::
    forall state input
    . [OnChainState state input]
    -> Either SMContractError (OnChainState state input)
defaultChooser [x] = Right x
defaultChooser xs  =
    let msg = "Found " <> show (length xs) <> " outputs, expected 1"
    in Left (ChooserError (Text.pack msg))

-- | A state chooser function that searches for an output with the thread token
--
-- TODO Remove uses old chain index
threadTokenChooserOld ::
    forall state input
    . Value
    -> [OnChainStateOld state input]
    -> Either SMContractError (OnChainStateOld state input)
threadTokenChooserOld val states =
    let hasToken OnChainStateOld{ocsTxOut'=TypedScriptTxOut{tyTxOutTxOut=Tx.TxOut{Tx.txOutValue}}} = val `Value.leq` txOutValue in
    case filter hasToken states of
        [x] -> Right x
        xs ->
            let msg = unwords ["Found", show (length xs), "outputs with thread token", show val, "expected 1"]
            in Left (ChooserError (Text.pack msg))

-- | A state chooser function that searches for an output with the thread token
threadTokenChooser ::
    forall state input
    . Value
    -> [OnChainState state input]
    -> Either SMContractError (OnChainState state input)
threadTokenChooser val states =
    let hasToken OnChainState{ocsTxOut=TypedScriptTxOut{tyTxOutTxOut=Tx.TxOut{Tx.txOutValue}}} = val `Value.leq` txOutValue in
    case filter hasToken states of
        [x] -> Right x
        xs ->
            let msg = unwords ["Found", show (length xs), "outputs with thread token", show val, "expected 1"]
            in Left (ChooserError (Text.pack msg))

-- | A state machine client with the 'defaultChooser' function
--
-- TODO Remove uses old chain index
mkStateMachineClientOld ::
    forall state input
    . SM.StateMachineInstance state input
    -> StateMachineClientOld state input
mkStateMachineClientOld inst =
    let threadTokenVal = SM.threadTokenValueOrZero inst
        scChooser' = if Value.isZero threadTokenVal then defaultChooserOld else threadTokenChooserOld threadTokenVal
    in StateMachineClientOld
        { scInstance' = inst
        , scChooser'
        }

mkStateMachineClient ::
    forall state input
    . SM.StateMachineInstance state input
    -> StateMachineClient state input
mkStateMachineClient inst =
    let threadTokenVal = SM.threadTokenValueOrZero inst
        scChooser = if Value.isZero threadTokenVal then defaultChooser else threadTokenChooser threadTokenVal
    in StateMachineClient
        { scInstance = inst
        , scChooser
        }

{-| Get the current on-chain state of the state machine instance.
    Return Nothing if there is no state on chain.
    Throws an @SMContractError@ if the number of outputs at the machine address is greater than one.
-}
-- | TODO: To delete. Uses the old chain index.
getOnChainStateOld ::
    ( AsSMContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    )
    => StateMachineClientOld state i
    -> Contract w schema e (Maybe (OnChainStateOld state i, UtxoMap))
getOnChainStateOld StateMachineClientOld{scInstance', scChooser'} = mapError (review _SMContractError) $ do
    utxo <- utxoAtOld (SM.machineAddress scInstance')
    let states = getStatesOld scInstance' utxo
    case states of
        [] -> pure Nothing
        _  -> case scChooser' states of
                Left err    -> throwing _SMContractError err
                Right state -> pure $ Just (state, utxo)

{-| Get the current on-chain state of the state machine instance.
    Return Nothing if there is no state on chain.
    Throws an @SMContractError@ if the number of outputs at the machine address is greater than one.
-}
getOnChainState ::
    ( AsSMContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    )
    => StateMachineClient state i
    -> Contract w schema e (Maybe (OnChainState state i, Map TxOutRef Tx.ChainIndexTxOut))
getOnChainState StateMachineClient{scInstance, scChooser} = mapError (review _SMContractError) $ do
    utxoTx <- utxosWithTxAt (SM.machineAddress scInstance)
    let states = getStates scInstance utxoTx
    case states of
        [] -> pure Nothing
        _  -> case scChooser states of
                Left err    -> throwing _SMContractError err
                Right state -> pure $ Just (state, fmap fst utxoTx)

-- | The outcome of 'waitForUpdateTimeout'
--
-- TODO Remove uses old chain index
data WaitingResultOld t i s
    = Timeout' t -- ^ The timeout happened before any change of the on-chain state was detected
    | ContractEnded' Tx.Tx i -- ^ The state machine instance ended
    | Transition' Tx.Tx i s -- ^ The state machine instance transitioned to a new state
    | InitialState' Tx.Tx s -- ^ The state machine instance was initialised
  deriving stock (Show,Generic,Functor)
  deriving anyclass (ToJSON, FromJSON)

-- | The outcome of 'waitForUpdateTimeout'
data WaitingResult t i s
    = Timeout t -- ^ The timeout happened before any change of the on-chain state was detected
    | ContractEnded ChainIndexTx i -- ^ The state machine instance ended
    | Transition ChainIndexTx i s -- ^ The state machine instance transitioned to a new state
    | InitialState ChainIndexTx s -- ^ The state machine instance was initialised
  deriving stock (Show,Generic,Functor)
  deriving anyclass (ToJSON, FromJSON)

-- | Wait for the on-chain state of the state machine instance to change until timeoutSlot,
--   and return the new state, or return 'ContractEnded' if the instance has been
--   terminated. If 'waitForUpdate' is called before the instance has even
--   started then it returns the first state of the instance as soon as it
--   has started.
--
-- TODO: To delete. Uses the old chain index
waitForUpdateUntilSlotOld ::
    ( AsSMContractError e
    , AsContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.FromData i
    )
    => StateMachineClientOld state i
    -> Slot
    -> Contract w schema e (WaitingResultOld Slot i state)
waitForUpdateUntilSlotOld client timeoutSlot = waitForUpdateTimeoutOld client (isSlot timeoutSlot) >>= fmap (fmap (tyTxOutData . ocsTxOut')) . awaitPromise

-- | Wait for the on-chain state of the state machine instance to change until timeoutSlot,
--   and return the new state, or return 'ContractEnded' if the instance has been
--   terminated. If 'waitForUpdate' is called before the instance has even
--   started then it returns the first state of the instance as soon as it
--   has started.
waitForUpdateUntilSlot ::
    ( AsSMContractError e
    , AsContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.FromData i
    )
    => StateMachineClient state i
    -> Slot
    -> Contract w schema e (WaitingResult Slot i state)
waitForUpdateUntilSlot client timeoutSlot = waitForUpdateTimeout client (isSlot timeoutSlot) >>= fmap (fmap (tyTxOutData . ocsTxOut)) . awaitPromise

-- | Same as 'waitForUpdateUntilSlot', but works with 'POSIXTime' instead.
--
-- | TODO: To delete. Uses the old chain index.
waitForUpdateUntilTimeOld ::
    ( AsSMContractError e
    , AsContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.FromData i
    )
    => StateMachineClientOld state i
    -> POSIXTime
    -> Contract w schema e (WaitingResultOld Slot i state)
waitForUpdateUntilTimeOld sm timeoutTime = waitForUpdateUntilSlotOld sm (TimeSlot.posixTimeToEnclosingSlot def timeoutTime)

-- | Same as 'waitForUpdateUntilSlot', but works with 'POSIXTime' instead.
waitForUpdateUntilTime ::
    ( AsSMContractError e
    , AsContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.FromData i
    )
    => StateMachineClient state i
    -> POSIXTime
    -> Contract w schema e (WaitingResult Slot i state)
waitForUpdateUntilTime sm timeoutTime = waitForUpdateUntilSlot sm (TimeSlot.posixTimeToEnclosingSlot def timeoutTime)

-- | Wait until the on-chain state of the state machine instance has changed,
--   and return the new state, or return 'Nothing' if the instance has been
--   terminated. If 'waitForUpdate' is called before the instance has even
--   started then it returns the first state of the instance as soon as it
--   has started.
--
-- | TODO: To delete. Uses the old chain index.
waitForUpdateOld ::
    forall state i w schema e.
    ( AsSMContractError e
    , AsContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.FromData i
    )
    => StateMachineClientOld state i
    -> Contract w schema e (Maybe (OnChainStateOld state i))
waitForUpdateOld client = waitForUpdateTimeoutOld client never >>= awaitPromise >>= \case
    Timeout' t        -> absurd t
    ContractEnded'{}  -> pure Nothing
    InitialState' _ r -> pure (Just r)
    Transition' _ _ r -> pure (Just r)

-- | Wait until the on-chain state of the state machine instance has changed,
--   and return the new state, or return 'Nothing' if the instance has been
--   terminated. If 'waitForUpdate' is called before the instance has even
--   started then it returns the first state of the instance as soon as it
--   has started.
waitForUpdate ::
    forall state i w schema e.
    ( AsSMContractError e
    , AsContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.FromData i
    )
    => StateMachineClient state i
    -> Contract w schema e (Maybe (OnChainState state i))
waitForUpdate client = waitForUpdateTimeout client never >>= awaitPromise >>= \case
    Timeout t        -> absurd t
    ContractEnded{}  -> pure Nothing
    InitialState _ r -> pure (Just r)
    Transition _ _ r -> pure (Just r)

-- | Construct a 'Promise' that waits for an update to the state machine's
--   on-chain state, or a user-defined timeout (whichever happens first).
--
-- TODO Delete, uses old chain index
waitForUpdateTimeoutOld ::
    forall state i t w schema e.
    ( AsSMContractError e
    , AsContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.FromData i
    )
    => StateMachineClientOld state i -- ^ The state machine client
    -> Promise w schema e t -- ^ The timeout
    -> Contract w schema e (Promise w schema e (WaitingResultOld t i (OnChainStateOld state i)))
waitForUpdateTimeoutOld client@StateMachineClientOld{scInstance', scChooser'} timeout = do
    currentState <- getOnChainStateOld client
    let success = case currentState of
                    Nothing ->
                        -- There is no on-chain state, so we wait for an output to appear
                        -- at the address. Any output that appears needs to be checked
                        -- with scChooser'
                        let addr = Scripts.validatorAddress $ typedValidator scInstance' in
                        promiseBind (utxoIsProducedOld addr) $ \txns -> do
                            let produced = getStatesOld @state @i scInstance' $ foldMap Ledger.toOutRefMap txns
                            case scChooser' produced of
                                Left e             -> throwing _SMContractError e
                                Right onChainState -> pure $ InitialState' (ocsTx' onChainState) onChainState
                    Just (OnChainStateOld{ocsTxOutRef'=Typed.TypedScriptTxOutRef{Typed.tyTxOutRefRef}, ocsTx'}, _) ->
                        promiseBind (utxoIsSpentOld tyTxOutRefRef) $ \txn -> do
                            let newStates = getStatesOld @state @i scInstance' (Ledger.toOutRefMap txn)
                                inp       = getInputOld tyTxOutRefRef (Ledger.eitherTx id id txn)
                            case (newStates, inp) of
                                ([], Just i) -> pure (ContractEnded' ocsTx' i)
                                (xs, Just i) -> case scChooser' xs of
                                    Left e         -> throwing _SMContractError e
                                    Right newState -> pure (Transition' ocsTx' i newState)
                                _ -> throwing_ _UnableToExtractTransition
    pure $ select success (Timeout' <$> timeout)

-- | Construct a 'Promise' that waits for an update to the state machine's
--   on-chain state, or a user-defined timeout (whichever happens first).
waitForUpdateTimeout ::
    forall state i t w schema e.
    ( AsSMContractError e
    , AsContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.FromData i
    )
    => StateMachineClient state i -- ^ The state machine client
    -> Promise w schema e t -- ^ The timeout
    -> Contract w schema e (Promise w schema e (WaitingResult t i (OnChainState state i)))
waitForUpdateTimeout client@StateMachineClient{scInstance, scChooser} timeout = do
    currentState <- getOnChainState client
    let success = case currentState of
                    Nothing ->
                        -- There is no on-chain state, so we wait for an output to appear
                        -- at the address. Any output that appears needs to be checked
                        -- with scChooser'
                        let addr = Scripts.validatorAddress $ typedValidator scInstance in
                        promiseBind (utxoIsProduced addr) $ \txns -> do
                            outRefMaps <- traverse toOutRefMap txns
                            let produced = getStates @state @i scInstance (Map.fromList $ concat $ toList outRefMaps)
                            case scChooser produced of
                                Left e             -> throwing _SMContractError e
                                Right onChainState -> pure $ InitialState (ocsTx onChainState) onChainState
                    Just (OnChainState{ocsTxOutRef=Typed.TypedScriptTxOutRef{Typed.tyTxOutRefRef}, ocsTx}, _) ->
                        promiseBind (utxoIsSpent tyTxOutRefRef) $ \txn -> do
                            outRefMap <- Map.fromList <$> toOutRefMap txn
                            let newStates = getStates @state @i scInstance outRefMap
                                inp       = getInput tyTxOutRefRef txn
                            case (newStates, inp) of
                                ([], Just i) -> pure (ContractEnded ocsTx i)
                                (xs, Just i) -> case scChooser xs of
                                    Left e         -> throwing _SMContractError e
                                    Right newState -> pure (Transition ocsTx i newState)
                                _ -> throwing_ _UnableToExtractTransition
    pure $ select success (Timeout <$> timeout)

toOutRefMap ::
    AsContractError e
    => ChainIndexTx
    -> Contract w s e [(TxOutRef, (Tx.ChainIndexTxOut, ChainIndexTx))]
toOutRefMap tx = catMaybes <$> mapM mkOutRef [0, fromIntegral $ length (_citxOutputs tx) - 1]
  where
    mkOutRef idx = do
      let outRef = Tx.TxOutRef (_citxTxId tx) idx
      ciTxOutM <- txOutFromRef outRef
      pure $ ciTxOutM >>= \ciTxOut -> pure (outRef, (ciTxOut, tx))

-- | Tries to run one step of a state machine: If the /guard/ (the last argument) returns @'Nothing'@ when given the
-- unbalanced transaction to be submitted, the old state and the new step, the step is run and @'Right'@ the new state is returned.
-- If the guard returns @'Just' a@, @'Left' a@ is returned instead.
--
-- TODO Delete, uses old chain index
runGuardedStepOld ::
    forall w a e state schema input.
    ( AsSMContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.ToData input
    )
    => StateMachineClientOld state input              -- ^ The state machine
    -> input                                       -- ^ The input to apply to the state machine
    -> (UnbalancedTx -> state -> state -> Maybe a) -- ^ The guard to check before running the step
    -> Contract w schema e (Either a (TransitionResult state input))
runGuardedStepOld = runGuardedStepWithOld mempty mempty

-- | Tries to run one step of a state machine: If the /guard/ (the last argument) returns @'Nothing'@ when given the
-- unbalanced transaction to be submitted, the old state and the new step, the step is run and @'Right'@ the new state is returned.
-- If the guard returns @'Just' a@, @'Left' a@ is returned instead.
runGuardedStep ::
    forall w a e state schema input.
    ( AsSMContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.ToData input
    )
    => StateMachineClient state input              -- ^ The state machine
    -> input                                       -- ^ The input to apply to the state machine
    -> (UnbalancedTx -> state -> state -> Maybe a) -- ^ The guard to check before running the step
    -> Contract w schema e (Either a (TransitionResult state input))
runGuardedStep = runGuardedStepWith mempty mempty

-- | Run one step of a state machine, returning the new state.
--
-- TODO Delete, uses old chain index
runStepOld ::
    forall w e state schema input.
    ( AsSMContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.ToData input
    )
    => StateMachineClientOld state input
    -- ^ The state machine
    -> input
    -- ^ The input to apply to the state machine
    -> Contract w schema e (TransitionResult state input)
runStepOld = runStepWithOld mempty mempty

-- | Run one step of a state machine, returning the new state.
runStep ::
    forall w e state schema input.
    ( AsSMContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.ToData input
    )
    => StateMachineClient state input
    -- ^ The state machine
    -> input
    -- ^ The input to apply to the state machine
    -> Contract w schema e (TransitionResult state input)
runStep = runStepWith mempty mempty

-- | Create a thread token. The thread token contains a reference to an unspent output of the wallet,
-- so it needs to used with 'mkStateMachine' immediately, and the machine must be initialised,
-- to prevent the output from getting spent in the mean time.
--
-- TODO Delete, uses old chain index
getThreadTokenOld :: AsSMContractError e => Contract w schema e ThreadToken
getThreadTokenOld = mapError (review _SMContractError) $ do
    txOutRef <- getUnspentOutputOld
    pure $ ThreadToken txOutRef (scriptCurrencySymbol (curPolicy txOutRef))

getThreadToken :: AsSMContractError e => Contract w schema e ThreadToken
getThreadToken = mapError (review _SMContractError) $ do
    txOutRef <- getUnspentOutput
    pure $ ThreadToken txOutRef (scriptCurrencySymbol (curPolicy txOutRef))

-- | Initialise a state machine
--
-- TODO Delete, uses old chain index
runInitialiseOld ::
    forall w e state schema input.
    ( PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.ToData input
    , AsSMContractError e
    )
    => StateMachineClientOld state input
    -- ^ The state machine
    -> state
    -- ^ The initial state
    -> Value
    -- ^ The value locked by the contract at the beginning
    -> Contract w schema e state
runInitialiseOld = runInitialiseWithOld mempty mempty

-- | Initialise a state machine
runInitialise ::
    forall w e state schema input.
    ( PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.ToData input
    , AsSMContractError e
    )
    => StateMachineClient state input
    -- ^ The state machine
    -> state
    -- ^ The initial state
    -> Value
    -- ^ The value locked by the contract at the beginning
    -> Contract w schema e state
runInitialise = runInitialiseWith mempty mempty

-- | Constraints & lookups needed to transition a state machine instance
data StateMachineTransition state input =
    StateMachineTransition
        { smtConstraints :: TxConstraints input state
        , smtOldState    :: State state
        , smtNewState    :: State state
        , smtLookups     :: ScriptLookups (StateMachine state input)
        }

-- | Initialise a state machine and supply additional constraints and lookups for transaction.
--
-- TODO Delete, uses old chain index
runInitialiseWithOld ::
    forall w e state schema input.
    ( PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.ToData input
    , AsSMContractError e
    )
    => ScriptLookups (StateMachine state input)
    -- ^ Additional lookups
    -> TxConstraints input state
    -- ^ Additional constraints
    -> StateMachineClientOld state input
    -- ^ The state machine
    -> state
    -- ^ The initial state
    -> Value
    -- ^ The value locked by the contract at the beginning
    -> Contract w schema e state
runInitialiseWithOld customLookups customConstraints StateMachineClientOld{scInstance'} initialState initialValue = mapError (review _SMContractError) $ do
    ownPK <- ownPubKey
    utxo <- utxoAtOld (Ledger.pubKeyAddress ownPK) -- TODO: use chain index
    let StateMachineInstance{stateMachine, typedValidator} = scInstance'
        constraints = mustPayToTheScript initialState (initialValue <> SM.threadTokenValueOrZero scInstance')
            <> foldMap ttConstraints (smThreadToken stateMachine)
            <> customConstraints
        red = Ledger.Redeemer (PlutusTx.toBuiltinData (Scripts.validatorHash typedValidator, Mint))
        ttConstraints ThreadToken{ttOutRef} =
            mustMintValueWithRedeemer red (SM.threadTokenValueOrZero scInstance')
            <> mustSpendPubKeyOutputOld ttOutRef
        lookups = Constraints.typedValidatorLookups typedValidator
            <> foldMap (mintingPolicy . curPolicy . ttOutRef) (smThreadToken stateMachine)
            <> Constraints.unspentOutputsOld utxo
            <> customLookups
    utx <- either (throwing _ConstraintResolutionError) pure (Constraints.mkTxOld lookups constraints)
    submitTxConfirmed utx
    pure initialState

-- | Initialise a state machine and supply additional constraints and lookups for transaction.
runInitialiseWith ::
    forall w e state schema input.
    ( PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.ToData input
    , AsSMContractError e
    )
    => ScriptLookups (StateMachine state input)
    -- ^ Additional lookups
    -> TxConstraints input state
    -- ^ Additional constraints
    -> StateMachineClient state input
    -- ^ The state machine
    -> state
    -- ^ The initial state
    -> Value
    -- ^ The value locked by the contract at the beginning
    -> Contract w schema e state
runInitialiseWith customLookups customConstraints StateMachineClient{scInstance} initialState initialValue = mapError (review _SMContractError) $ do
    ownPK <- ownPubKey
    utxo <- utxosAt (Ledger.pubKeyAddress ownPK) -- TODO: use chain index
    let StateMachineInstance{stateMachine, typedValidator} = scInstance
        constraints = mustPayToTheScript initialState (initialValue <> SM.threadTokenValueOrZero scInstance)
            <> foldMap ttConstraints (smThreadToken stateMachine)
            <> customConstraints
        red = Ledger.Redeemer (PlutusTx.toBuiltinData (Scripts.validatorHash typedValidator, Mint))
        ttConstraints ThreadToken{ttOutRef} =
            mustMintValueWithRedeemer red (SM.threadTokenValueOrZero scInstance)
            <> mustSpendPubKeyOutput ttOutRef
        lookups = Constraints.typedValidatorLookups typedValidator
            <> foldMap (mintingPolicy . curPolicy . ttOutRef) (smThreadToken stateMachine)
            <> Constraints.unspentOutputs utxo
            <> customLookups
    utx <- either (throwing _ConstraintResolutionError) pure (Constraints.mkTx lookups constraints)
    submitTxConfirmed utx
    pure initialState

-- | Run one step of a state machine, returning the new state. We can supply additional constraints and lookups for transaction.
--
-- TODO Remove uses old chain index
runStepWithOld ::
    forall w e state schema input.
    ( AsSMContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.ToData input
    )
    => ScriptLookups (StateMachine state input)
    -- ^ Additional lookups
    -> TxConstraints input state
    -- ^ Additional constraints
    -> StateMachineClientOld state input
    -- ^ The state machine
    -> input
    -- ^ The input to apply to the state machine
    -> Contract w schema e (TransitionResult state input)
runStepWithOld lookups constraints smc input =
    runGuardedStepWithOld lookups constraints smc input (\_ _ _ -> Nothing) >>= pure . \case
        Left a  -> absurd a
        Right a -> a

-- | Run one step of a state machine, returning the new state. We can supply additional constraints and lookups for transaction.
runStepWith ::
    forall w e state schema input.
    ( AsSMContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.ToData input
    )
    => ScriptLookups (StateMachine state input)
    -- ^ Additional lookups
    -> TxConstraints input state
    -- ^ Additional constraints
    -> StateMachineClient state input
    -- ^ The state machine
    -> input
    -- ^ The input to apply to the state machine
    -> Contract w schema e (TransitionResult state input)
runStepWith lookups constraints smc input =
    runGuardedStepWith lookups constraints smc input (\_ _ _ -> Nothing) >>= pure . \case
        Left a  -> absurd a
        Right a -> a

-- | The same as 'runGuardedStep' but we can supply additional constraints and lookups for transaction.
--
-- TODO: Delete. Uses old chain index
runGuardedStepWithOld ::
    forall w a e state schema input.
    ( AsSMContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.ToData input
    )
    => ScriptLookups (StateMachine state input)    -- ^ Additional lookups
    -> TxConstraints input state                   -- ^ Additional constraints
    -> StateMachineClientOld state input              -- ^ The state machine
    -> input                                       -- ^ The input to apply to the state machine
    -> (UnbalancedTx -> state -> state -> Maybe a) -- ^ The guard to check before running the step
    -> Contract w schema e (Either a (TransitionResult state input))
runGuardedStepWithOld userLookups userConstraints smc input guard = mapError (review _SMContractError) $ mkStepOld smc input >>= \case
     Right StateMachineTransition{smtConstraints,smtOldState=State{stateData=os}, smtNewState=State{stateData=ns}, smtLookups} -> do
         pk <- ownPubKey
         let lookups = smtLookups { Constraints.slOwnPubkey = Just $ pubKeyHash pk }
         utx <- either (throwing _ConstraintResolutionError) pure (Constraints.mkTxOld (lookups <> userLookups) (smtConstraints <> userConstraints))
         case guard utx os ns of
             Nothing -> do
                 submitTxConfirmed utx
                 pure $ Right $ TransitionSuccess ns
             Just a  -> pure $ Left a
     Left e -> pure $ Right $ TransitionFailure e

-- | The same as 'runGuardedStep' but we can supply additional constraints and lookups for transaction.
runGuardedStepWith ::
    forall w a e state schema input.
    ( AsSMContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.ToData input
    )
    => ScriptLookups (StateMachine state input)    -- ^ Additional lookups
    -> TxConstraints input state                   -- ^ Additional constraints
    -> StateMachineClient state input              -- ^ The state machine
    -> input                                       -- ^ The input to apply to the state machine
    -> (UnbalancedTx -> state -> state -> Maybe a) -- ^ The guard to check before running the step
    -> Contract w schema e (Either a (TransitionResult state input))
runGuardedStepWith userLookups userConstraints smc input guard = mapError (review _SMContractError) $ mkStep smc input >>= \case
     Right StateMachineTransition{smtConstraints,smtOldState=State{stateData=os}, smtNewState=State{stateData=ns}, smtLookups} -> do
         pk <- ownPubKey
         let lookups = smtLookups { Constraints.slOwnPubkey = Just $ pubKeyHash pk }
         utx <- either (throwing _ConstraintResolutionError) pure (Constraints.mkTx (lookups <> userLookups) (smtConstraints <> userConstraints))
         case guard utx os ns of
             Nothing -> do
                 submitTxConfirmed utx
                 pure $ Right $ TransitionSuccess ns
             Just a  -> pure $ Left a
     Left e -> pure $ Right $ TransitionFailure e

-- | Given a state machine client and an input to apply to
--   the client's state machine instance, compute the 'StateMachineTransition'
--   that can produce an actual transaction performing the transition
--
-- TODO Remove uses old chain index
mkStepOld ::
    forall w e state schema input.
    ( AsSMContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    )
    => StateMachineClientOld state input
    -> input
    -> Contract w schema e (Either (InvalidTransition state input) (StateMachineTransition state input))
mkStepOld client@StateMachineClientOld{scInstance'} input = do
    let StateMachineInstance{stateMachine, typedValidator} = scInstance'
        StateMachine{smTransition} = stateMachine
    maybeState <- getOnChainStateOld client
    case maybeState of
        Nothing -> pure $ Left $ InvalidTransition Nothing input
        Just (onChainState, utxo) -> do
            let OnChainStateOld{ocsTxOut'=TypedScriptTxOut{tyTxOutData=currentState, tyTxOutTxOut}, ocsTxOutRef'} = onChainState
                oldState = State
                    { stateData = currentState
                      -- Hide the thread token value from the client code
                    , stateValue = Ledger.txOutValue tyTxOutTxOut <> inv (SM.threadTokenValueOrZero scInstance')
                    }
                inputConstraints = [InputConstraint{icRedeemer=input, icTxOutRef = Typed.tyTxOutRefRef ocsTxOutRef' }]

            case smTransition oldState input of
                Just (newConstraints, newState)  ->
                    let isFinal = smFinal stateMachine (stateData newState)
                        lookups =
                            Constraints.typedValidatorLookups typedValidator
                            <> Constraints.unspentOutputsOld utxo
                            <> if isFinal then foldMap (mintingPolicy . curPolicy . ttOutRef) (smThreadToken stateMachine) else mempty
                        red = Ledger.Redeemer (PlutusTx.toBuiltinData (Scripts.validatorHash typedValidator, Burn))
                        unmint = if isFinal then mustMintValueWithRedeemer red (inv $ SM.threadTokenValueOrZero scInstance') else mempty
                        outputConstraints =
                            [ OutputConstraint
                                { ocDatum = stateData newState
                                  -- Add the thread token value back to the output
                                , ocValue = stateValue newState <> SM.threadTokenValueOrZero scInstance'
                                }
                            | not isFinal ]
                    in pure
                        $ Right
                        $ StateMachineTransition
                            { smtConstraints =
                                (newConstraints <> unmint)
                                    { txOwnInputs = inputConstraints
                                    , txOwnOutputs = outputConstraints
                                    }
                            , smtOldState = oldState
                            , smtNewState = newState
                            , smtLookups = lookups
                            }
                Nothing -> pure $ Left $ InvalidTransition (Just oldState) input

-- | Given a state machine client and an input to apply to
--   the client's state machine instance, compute the 'StateMachineTransition'
--   that can produce an actual transaction performing the transition
mkStep ::
    forall w e state schema input.
    ( AsSMContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    )
    => StateMachineClient state input
    -> input
    -> Contract w schema e (Either (InvalidTransition state input) (StateMachineTransition state input))
mkStep client@StateMachineClient{scInstance} input = do
    let StateMachineInstance{stateMachine, typedValidator} = scInstance
        StateMachine{smTransition} = stateMachine
    maybeState <- getOnChainState client
    case maybeState of
        Nothing -> pure $ Left $ InvalidTransition Nothing input
        Just (onChainState, utxo) -> do
            let OnChainState{ocsTxOut=TypedScriptTxOut{tyTxOutData=currentState, tyTxOutTxOut}, ocsTxOutRef} = onChainState
                oldState = State
                    { stateData = currentState
                      -- Hide the thread token value from the client code
                    , stateValue = Ledger.txOutValue tyTxOutTxOut <> inv (SM.threadTokenValueOrZero scInstance)
                    }
                inputConstraints = [InputConstraint{icRedeemer=input, icTxOutRef = Typed.tyTxOutRefRef ocsTxOutRef }]

            case smTransition oldState input of
                Just (newConstraints, newState)  ->
                    let isFinal = smFinal stateMachine (stateData newState)
                        lookups =
                            Constraints.typedValidatorLookups typedValidator
                            <> Constraints.unspentOutputs utxo
                            <> if isFinal then foldMap (mintingPolicy . curPolicy . ttOutRef) (smThreadToken stateMachine) else mempty
                        red = Ledger.Redeemer (PlutusTx.toBuiltinData (Scripts.validatorHash typedValidator, Burn))
                        unmint = if isFinal then mustMintValueWithRedeemer red (inv $ SM.threadTokenValueOrZero scInstance) else mempty
                        outputConstraints =
                            [ OutputConstraint
                                { ocDatum = stateData newState
                                  -- Add the thread token value back to the output
                                , ocValue = stateValue newState <> SM.threadTokenValueOrZero scInstance
                                }
                            | not isFinal ]
                    in pure
                        $ Right
                        $ StateMachineTransition
                            { smtConstraints =
                                (newConstraints <> unmint)
                                    { txOwnInputs = inputConstraints
                                    , txOwnOutputs = outputConstraints
                                    }
                            , smtOldState = oldState
                            , smtNewState = newState
                            , smtLookups = lookups
                            }
                Nothing -> pure $ Left $ InvalidTransition (Just oldState) input

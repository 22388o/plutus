{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

-- | Reduced example of the SM contract to reproduce the token handling in and around 'runStep'.
module Spec.ThreadToken where

import           PlutusTx.Prelude             hiding (Eq)
import           Prelude                      (Show, String, show)

import           Control.Monad                (void)
import           GHC.Generics                 (Generic)
import           Ledger.Typed.Scripts         (TypedValidator, mkTypedValidator)
import qualified Ledger.Typed.Scripts         as Scripts
import           Plutus.Contract              (Contract, EmptySchema, logError, mapError)
import           Plutus.Contract.StateMachine (StateMachine, StateMachineClient, StateMachineClientOld, ThreadToken,
                                               mkStateMachine, stateData)
import qualified Plutus.Contract.StateMachine as SM
import           Plutus.Contract.Test
import           Plutus.Trace                 (EmulatorTrace, activateContractWallet)
import qualified Plutus.Trace                 as Trace
import qualified PlutusTx

import           Test.Tasty

-- * Very simple plutus state machine using a thread token

data State
  = First
  | Second
  deriving (Generic, Show)

PlutusTx.makeLift ''State
PlutusTx.unstableMakeIsData ''State

data Input
  = Step
  deriving (Generic, Show)
PlutusTx.makeLift ''Input
PlutusTx.unstableMakeIsData ''Input

{-# INLINEABLE transition #-}
transition :: SM.State State -> Input -> Maybe (SM.TxConstraints SM.Void SM.Void, SM.State State)
transition oldState _ = Just (mempty, oldState{stateData = Second})

{-# INLINEABLE stateMachine #-}
stateMachine :: ThreadToken -> StateMachine State Input
stateMachine threadToken =
  mkStateMachine (Just threadToken) transition isFinal
 where
  isFinal = const False

typedValidator :: ThreadToken -> TypedValidator (StateMachine State Input)
typedValidator threadToken =
  mkTypedValidator @(StateMachine State Input)
    ($$(PlutusTx.compile [||validator||]) `PlutusTx.applyCode` PlutusTx.liftCode threadToken)
    $$(PlutusTx.compile [||wrap||])
 where
  validator c = SM.mkValidator (stateMachine c)
  wrap = Scripts.wrapValidator @State @Input

-- TODO: Delete, uses old chain index
stateMachineClientOld :: ThreadToken -> StateMachineClientOld State Input
stateMachineClientOld threadToken =
  let machine = stateMachine threadToken
      inst = typedValidator threadToken
   in SM.mkStateMachineClientOld (SM.StateMachineInstance machine inst)

stateMachineClient :: ThreadToken -> StateMachineClient State Input
stateMachineClient threadToken =
  let machine = stateMachine threadToken
      inst = typedValidator threadToken
   in SM.mkStateMachineClient (SM.StateMachineInstance machine inst)
-- * Minimal test runner for repro

-- TODO: To delete. Uses the old chain index.
contractOld :: Contract () EmptySchema String ()
contractOld = do
  threadToken <- mapSMError SM.getThreadTokenOld
  logError @String $ "Forged thread token: " <> show threadToken

  let client = stateMachineClientOld threadToken
  void $ mapSMError $ SM.runInitialiseOld client First mempty
  logError @String $ "Initialized state machine"

  res <- mapSMError $ SM.runStepOld client Step
  case res of
    SM.TransitionFailure (SM.InvalidTransition os i) -> logError @String $ "Invalid transition: " <> show (os, i)
    SM.TransitionSuccess s                           -> logError @String $ "Transition success: " <> show s
 where
  mapSMError = mapError (show @SM.SMContractError)

contract :: Contract () EmptySchema String ()
contract = do
  threadToken <- mapSMError SM.getThreadToken
  logError @String $ "Forged thread token: " <> show threadToken

  let client = stateMachineClient threadToken
  void $ mapSMError $ SM.runInitialise client First mempty
  logError @String $ "Initialized state machine"

  res <- mapSMError $ SM.runStep client Step
  case res of
    SM.TransitionFailure (SM.InvalidTransition os i) -> logError @String $ "Invalid transition: " <> show (os, i)
    SM.TransitionSuccess s                           -> logError @String $ "Transition success: " <> show s
 where
  mapSMError = mapError (show @SM.SMContractError)

-- TODO: To delete. Uses the old chain index.
testTraceOld :: EmulatorTrace ()
testTraceOld = do
  void $ activateContractWallet (Wallet 1) contractOld
  void $ Trace.waitNSlots 10

testTrace :: EmulatorTrace ()
testTrace = do
  void $ activateContractWallet (Wallet 1) contract
  void $ Trace.waitNSlots 10

tests :: TestTree
tests = testGroup "Thread Token"
    [ checkPredicateOld "Runs successfully"
        (assertDone contractOld (Trace.walletInstanceTag (Wallet 1)) (const True) "No errors"
         .&&. assertNoFailedTransactions)
        testTraceOld
    , checkPredicate "Runs successfully"
        (assertDone contract (Trace.walletInstanceTag (Wallet 1)) (const True) "No errors"
         .&&. assertNoFailedTransactions)
        testTrace
    ]

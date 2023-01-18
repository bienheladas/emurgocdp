
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Beneficiary.EmulatorBeneficiary where

import Control.Monad.Freer.Extras      as Extras
import Data.Default                 (Default(..))
import Data.Functor                       (void)

import Ledger.TimeSlot                 as TimeSlot
import Plutus.Trace
import Wallet.Emulator.Wallet          as Wallet
import qualified Plutus.Trace.Emulator as Emulator
--import qualified Plutus.Contract.Test as ContractTest
--import qualified Wallet.Emulator.Folds as WalletFolds
--import qualified Ledger.Ada as Ada

import qualified Beneficiary.OffChain  as OffChain

test :: IO ()
test = Emulator.runEmulatorTraceIO myTrace

myTrace :: Emulator.EmulatorTrace ()
myTrace = do
    --Enviar ADA al contrato con el endpoint "produce"
    h1 <- Emulator.activateContractWallet (knownWallet 1) OffChain.endpoints
    h2 <- Emulator.activateContractWallet (knownWallet 2) OffChain.endpoints
    h3 <- Emulator.activateContractWallet (knownWallet 3) OffChain.endpoints

    Emulator.callEndpoint @"produce" h1 $ OffChain.ProduceParams {
        OffChain.ppCreator = Wallet.mockWalletPaymentPubKeyHash $ knownWallet 3
      , OffChain.ppBeneficiary = Wallet.mockWalletPaymentPubKeyHash $ knownWallet 2
      , OffChain.ppDeadline = TimeSlot.slotToBeginPOSIXTime def 20
      , OffChain.ppGuess = 3000
      , OffChain.ppAmount = 30000000
    }

    --1. Beneficiario equivocado
    --2. Transaccion fuera de fecha limite
    --3. Fallo en adivinar el numero

    void $ waitUntilSlot 2
    Emulator.callEndpoint @"consume" h2 $ OffChain.ConsumeParams {
        OffChain.getRedeem = 3000
    }
    void $ waitUntilSlot 20
    Emulator.callEndpoint @"consume" h3 $ OffChain.ConsumeParams {
        OffChain.getRedeem = 3000
    }
    void $ waitUntilSlot 1
    Emulator.callEndpoint @"consume" h2 $ OffChain.ConsumeParams {
        OffChain.getRedeem = 300
    }
    void $ waitUntilSlot 1
    Emulator.callEndpoint @"consume" h2 $ OffChain.ConsumeParams {
        OffChain.getRedeem = 3000
    }
    s <- waitNSlots 2
    Extras.logInfo $ "alcanzado " ++ show s
    

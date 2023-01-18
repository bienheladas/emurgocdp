-- 1. dependecies
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-} 
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}

-- 2. imports external/imports

module Beneficiary.OnChain where

import qualified Prelude                                as P
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as Scripts
import qualified Plutus.Script.Utils.V2.Scripts as V2UtilsScripts
import qualified Plutus.V2.Ledger.Contexts                       as Contexts
import qualified Plutus.V2.Ledger.Api                            as LedgerApiV2
import qualified Plutus.V1.Ledger.Interval                            as LedgerIntervalV1
import qualified Ledger                                          
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Ledger.Ada                                      as Ada

data Beneficiary = Beneficiary
    { creator :: Ledger.PaymentPubKeyHash
      , beneficiary :: Ledger.PaymentPubKeyHash
      , deadline :: LedgerApiV2.POSIXTime
      , guess :: Integer
    }

PlutusTx.unstableMakeIsData ''Beneficiary 

newtype Redeem = Redeem 
    {
      redeem :: Integer
    } deriving P.Show

PlutusTx.unstableMakeIsData ''Redeem 

data Benef
instance Scripts.ValidatorTypes Benef where
    type instance RedeemerType Benef = Redeem
    type instance DatumType Benef = Beneficiary

--3. Onchain Code (validator)
{-# INLINABLE benefValidator #-}

--simple :: datum -> redeemer -> context -> Bool
benefValidator :: Beneficiary -> Redeem -> Contexts.ScriptContext -> Bool
benefValidator d r context = 
  traceIfFalse "Fallo en adivinar" (guess d == redeem r) &&
  traceIfFalse "No fue firmado por el beneficiario" signedByBenefeciary &&
  traceIfFalse "No se alcanzo la fecha limite" deadlinePassed &&
  traceIfFalse "No hay alcanza para las regalias al creador" calculateRoyalties
  where
    txinfo :: Contexts.TxInfo
    txinfo = Contexts.scriptContextTxInfo context

    signedByBenefeciary :: Bool
    signedByBenefeciary = Contexts.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash (beneficiary d)

    deadlinePassed :: Bool
    deadlinePassed = LedgerIntervalV1.contains (LedgerIntervalV1.from (deadline d)) (Contexts.txInfoValidRange txinfo)

    calculateRoyalties :: Bool
    calculateRoyalties = validateRoyalties d txinfo --(from the txinfo -> total amount of the txin to build the tx)
    
benefCompile :: Scripts.TypedValidator Benef
benefCompile = Scripts.mkTypedValidator @Benef
      $$(PlutusTx.compile [|| benefValidator ||])
      $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.mkUntypedValidator @Beneficiary @Redeem

validator :: V2UtilsScripts.Validator
validator = Scripts.validatorScript benefCompile

simpleHash :: V2UtilsScripts.ValidatorHash
simpleHash = V2UtilsScripts.validatorHash validator

simpleAddress :: Ledger.Address
simpleAddress = Ledger.scriptHashAddress simpleHash

--4.
{-# INLINABLE validateRoyalties #-}
validateRoyalties :: Beneficiary -> Contexts.TxInfo -> Bool
validateRoyalties d txinfo = compareValues (getValuePaidToCreator d txinfo) (getTotalInputValue txinfo)

{-# INLINABLE getValuePaidToCreator #-}
getValuePaidToCreator :: Beneficiary -> Contexts.TxInfo -> Ada.Ada 
getValuePaidToCreator d txinfo = Ada.fromValue $ Contexts.valuePaidTo txinfo (Ledger.unPaymentPubKeyHash (creator d))

{-# INLINABLE getTotalInputValue #-}
getTotalInputValue :: Contexts.TxInfo -> Ada.Ada
getTotalInputValue txinfo = Ada.fromValue $ Contexts.valueSpent txinfo

{-# INLINABLE compareValues #-}
compareValues :: Ada.Ada -> Ada.Ada -> Bool
compareValues vToCreator vTotal = vToCreator >= vTotal `Ada.divide` 10

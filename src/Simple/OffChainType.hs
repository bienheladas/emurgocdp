--1. Dependencias
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

--2. Importaciones
module Simple.OffChainType where

--Haskell Imports
import qualified Control.Monad               as Monad (void)
import qualified Data.Map                    as Map

--Plutus imports
import qualified Plutus.Contract             as PlutusContract
import qualified Prelude                     as P
--import qualified Text.Printf                 as TextPrintf (printf)
import           PlutusTx.Prelude
import           Ledger.Ada                  as Ada
import qualified Ledger.Constraints          as Constraints
import qualified Ledger.Tx                   as LedgerTx
import qualified Data.Void                   as Void (Void)
import qualified PlutusTx
import qualified Plutus.V1.Ledger.Scripts    as ScriptsLedger
import qualified Data.Text                   as DT (Text)

import qualified Simple.SimpleType           as OnChain

type SimpleTypeSchema =
    PlutusContract.Endpoint "produce" Integer
    PlutusContract..\/ PlutusContract.Endpoint "consume" Integer

produce :: forall w s e. PlutusContract.AsContractError e => Integer -> PlutusContract.Contract w s e ()
produce value = do
    PlutusContract.logInfo @P.String "-------------------------------Inicio de endpoint produce-------------------------"
    let tx = Constraints.mustPayToOtherScript (OnChain.simpleHash) (ScriptsLedger.Datum $ PlutusTx.toBuiltinData ()) (lovelaceValueOf value)
        lookups = Constraints.plutusV2OtherScript OnChain.validator
    
    submittedTx <- PlutusContract.submitTxConstraintsWith @Void.Void lookups tx
    Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx
    PlutusContract.logInfo @P.String "-------------------------------Inicio de endpoint produce-------------------------"

consume :: forall w s e. PlutusContract.AsContractError e => Integer -> PlutusContract.Contract w s e ()
consume redeem = do
    if redeem == 1500
        then do 
            utxos <- PlutusContract.utxosAt OnChain.simpleAddress
            let orefs = fst <$> Map.toList utxos
                lookups = Constraints.unspentOutputs utxos P.<>
                        Constraints.plutusV2OtherScript OnChain.validator
                tx :: Constraints.TxConstraints Void.Void Void.Void
                tx = mconcat [Constraints.mustSpendScriptOutput oref $ ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData redeem | oref <- orefs]
            submittedTx <- PlutusContract.submitTxConstraintsWith @Void.Void lookups tx
            Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx
            PlutusContract.logInfo @P.String "-------------------------------Adivino correctamente!-------------------------"
    else PlutusContract.logInfo @P.String "-------------------------------No adivino correctamente!-------------------------"

endpoints :: PlutusContract.Contract () SimpleTypeSchema DT.Text ()
endpoints = PlutusContract.awaitPromise (produce' `PlutusContract.select` consume') >> endpoints
   where 
    produce' = PlutusContract.endpoint @"produce" produce
    consume' = PlutusContract.endpoint @"consume" consume
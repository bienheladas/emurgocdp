--1. Dependencias
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

--2. Importaciones
module Simple.SimpleFail where

import PlutusTx.Prelude
import qualified Ledger
import qualified Plutus.V2.Ledger.Api as Scripts
import qualified Plutus.Script.Utils.V2.Scripts as V2UtilsScripts
import PlutusTx

--3. Onchain Code (validator)
{-# INLINABLE simple #-}

--simple :: datum -> redeemer -> context -> Bool
simple :: BuiltinData -> BuiltinData -> BuiltinData -> ()
--simple _ _ _ = traceIfFalse "Falló" funcionQueEvaluaABool
simple _ _ _ = error ()

simpleCompile :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
simpleCompile = $$(compile [|| simple ||])

validator :: V2UtilsScripts.Validator
validator = Scripts.mkValidatorScript simpleCompile

simpleHash :: V2UtilsScripts.ValidatorHash
simpleHash = V2UtilsScripts.validatorHash validator

simpleAddress :: Ledger.Address
simpleAddress = Ledger.scriptHashAddress simpleHash

--4.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Helper.Compiler1 where

import qualified Simple.Simple as OnChain

import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..))

import qualified Plutus.V2.Ledger.Api as LedgerApiV2

writeValidator :: FilePath -> LedgerApiV2.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . LedgerApiV2.unValidatorScript

writeTheFile :: IO (Either (FileError ()) ())
writeTheFile = writeValidator "src/Simple/plutus_scripts/Simple.plutus" $ OnChain.validator

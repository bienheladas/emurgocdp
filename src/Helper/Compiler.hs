module Helper.Compiler where

import qualified Simple.Simple as Simple

import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as Base16

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)

import qualified Plutus.V2.Ledger.Api as LedgerApiV2
import qualified Cardano.Binary as CBOR

script ::LedgerApiV2.Script
script = LedgerApiV2.unValidatorScript Simple.validator

scriptShortBS :: SBS.ShortByteString
scriptShortBS = (SBS.toShort . LBS.toStrict . serialise) script

srlScript :: PlutusScript PlutusScriptV2
srlScript = PlutusScriptSerialised scriptShortBS

scriptCBORHex :: B.ByteString
scriptCBORHex = Base16.encode $ CBOR.serialize' srlScript

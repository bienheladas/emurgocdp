{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Parameterized.Deploy where


import qualified Data.ByteString.Char8                   as B
import qualified Data.ByteString.Lazy                    as LBS
import qualified Data.ByteString.Short                   as SBS
import qualified Data.ByteString.Base16                  as B16

import           Cardano.Api
import           Cardano.Api.Shelley (PlutusScript (..))
import           Codec.Serialise (serialise)
import qualified Data.Aeson                              as DataAeson
import qualified Plutus.V2.Ledger.Api                    as LedgerApiV2
import qualified PlutusTx
import qualified Ledger
import qualified PlutusTx.Prelude                        as PlutusPrelude 

import qualified Parameterized.OnChain    as OnChain
-- import qualified Helper.GetSlot                                      as GetSlot

--------------------------------------------------------------------
-- Section to insert the Paramemters to initialize the contract
--------------------------------------------------------------------

creatorHash :: B.ByteString
creatorHash = "81a329fde038c41a812420d97e894c0f954376c6fa50c770a631954c"

beneficiaryHash :: B.ByteString
beneficiaryHash = "d01ad23175ae1159209f97e9275ffaa85404988b58c76742cddcb5bd"

paramDeadline :: Ledger.POSIXTime
paramDeadline = 1673129000000

parameters :: OnChain.BeneParam
parameters = OnChain.BeneParam
    {
        OnChain.creator = convertToPubKeyHash creatorHash,
        OnChain.beneficiary = convertToPubKeyHash beneficiaryHash,
        OnChain.deadline = paramDeadline
    }

--------------------------------------------------------------------
-- Some Helper functions
--------------------------------------------------------------------
decodeHex :: B.ByteString -> PlutusPrelude.BuiltinByteString
decodeHex hexBS =
    case getTx of
        Right decHex -> do
            PlutusPrelude.toBuiltin(decHex)  
        Left _ -> do
            PlutusPrelude.emptyByteString 
    where
        getTx :: Either String B.ByteString 
        getTx = B16.decode hexBS

convertToPubKeyHash :: B.ByteString -> Ledger.PaymentPubKeyHash
convertToPubKeyHash b = Ledger.PaymentPubKeyHash (Ledger.PubKeyHash $ decodeHex b)

--------------------------------------------------------------------
-- Datum and Redeemer creation
--------------------------------------------------------------------

guessNumber :: Integer
guessNumber = 8566543223

datumGuess :: OnChain.Dat
datumGuess = OnChain.Dat {OnChain.guess = guessNumber}

main :: IO()
main = do
    writeDatumUnit
    writeContributorDatum
    _ <- writeParameterized



    return ()

dataToScriptData :: LedgerApiV2.Data -> ScriptData
dataToScriptData (LedgerApiV2.Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (LedgerApiV2.List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.I n)         = ScriptDataNumber n
dataToScriptData (LedgerApiV2.B bs)        = ScriptDataBytes bs


writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . DataAeson.encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeValidator :: FilePath -> LedgerApiV2.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . LedgerApiV2.unValidatorScript

writeDatumUnit :: IO ()
writeDatumUnit = writeJSON "src/Parameterized/Deploy/unit.json" ()

writeContributorDatum :: IO ()
writeContributorDatum = 
    let contributor = datumGuess
        d = PlutusTx.toBuiltinData contributor
    in writeJSON "src/Parameterized/Deploy/parameterized-datum.json" d

writeParameterized :: IO (Either (FileError ()) ())
writeParameterized = writeValidator "src/Parameterized/Deploy/Parameterized.plutus" $ OnChain.validator parameters

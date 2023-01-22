{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}

module SoccerFieldMarket.Onchain
    ( apiBuyScript
    , buyScriptAsShortBs
    , typedBuyValidator
    , Sale
    , buyValidator
    , buyValidatorHash
    , nftDatum
    ) where

import qualified Data.ByteString.Lazy  as LB
import qualified Data.ByteString.Short as SBS
import           Codec.Serialise       ( serialise )

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import qualified PlutusTx
import PlutusTx.Prelude as Plutus
    ( Bool(..), Eq((==)), (.), (&&), Integer, Maybe(..), (>=), fromInteger, traceIfFalse)
import Ledger
    ( PubKeyHash(..),
      --ValidatorHash,
      --Address(Address),
      --validatorHash,
      DatumHash,
      Datum(..),
      txOutDatum,
      txSignedBy,
      ScriptContext(scriptContextTxInfo),
      --ownHash,
      TxInfo,
      --Validator,
      --TxOut,
      txInfoSignatories,
      unValidatorScript,
      --txInInfoResolved,
      --txInfoInputs,
      valuePaidTo,
      --txOutAddress,
      txInfoValidRange)
import qualified Ledger.Typed.Scripts         as Scripts
import qualified Plutus.V1.Ledger.Scripts     as Plutus
import           Ledger.Value                 as Value ( valueOf )
import qualified Ledger.Ada         as Ada (fromValue, Ada (getLovelace))
--import           Plutus.V1.Ledger.Credential (Credential(ScriptCredential))
import qualified Plutus.V1.Ledger.Interval    as LedgerIntervalV1
--import qualified Plutus.V2.Ledger.Contexts    as Contexts
import qualified Plutus.V1.Ledger.Tx          as LedgerV1
import qualified Plutus.Script.Utils.V2.Scripts as V2UtilsScripts


import SoccerFieldMarket.Types    (NFTSale(..), SaleAction(..), MarketParams(..))


{-# INLINABLE nftDatum #-}
nftDatum :: LedgerV1.TxOut -> (DatumHash -> Maybe Datum) -> Maybe NFTSale
nftDatum o f = do
    dh <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

{-# INLINABLE mkBuyValidator #-}
mkBuyValidator :: MarketParams -> NFTSale -> SaleAction -> ScriptContext -> Bool
mkBuyValidator _ nfts r ctx = case r of
    Buy     -> traceIfFalse "Deadline reached" deadlinepassed &&
               traceIfFalse "The token amount buyed is incorrect" (valueOf (valuePaidTo txinfo buyer) (nCurrency nfts) (nToken nfts) == 1) &&
               traceIfFalse "The ADA amount payed to seller is incorrect" (checkSellerOut (nRenter nfts) (nPrice nfts))
               
    Close   -> traceIfFalse "The Tx is not signed by the seller" (txSignedBy (scriptContextTxInfo ctx) (nRenter nfts))

  where
    txinfo :: TxInfo
    txinfo = scriptContextTxInfo ctx

    buyer :: PubKeyHash
    buyer = case txInfoSignatories txinfo of
            [pubKeyHash] -> pubKeyHash

    --checkSingleBuy :: Bool
    --checkSingleBuy = let is = [ i | i <- map txInInfoResolved (txInfoInputs txinfo), txOutAddress i == Address (ScriptCredential $ ownHash ctx) Nothing ] in
    --    length is == 1

    checkSellerOut :: PubKeyHash -> Integer -> Bool
    checkSellerOut seller price = fromInteger (Ada.getLovelace (Ada.fromValue (valuePaidTo txinfo seller))) >= fromInteger price

    deadlinepassed :: Bool 
    deadlinepassed = LedgerIntervalV1.contains (LedgerIntervalV1.to (nDeadline nfts)) (txInfoValidRange txinfo)

data Sale
instance Scripts.ValidatorTypes Sale where
    type instance DatumType Sale    = NFTSale
    type instance RedeemerType Sale = SaleAction


typedBuyValidator :: MarketParams -> Scripts.TypedValidator Sale
typedBuyValidator mp = Scripts.mkTypedValidator @Sale
    ($$(PlutusTx.compile [|| mkBuyValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode mp)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.mkUntypedValidator @NFTSale @SaleAction

buyValidator :: MarketParams -> V2UtilsScripts.Validator
buyValidator = Scripts.validatorScript . typedBuyValidator

buyValidatorHash :: MarketParams -> V2UtilsScripts.ValidatorHash
buyValidatorHash = V2UtilsScripts.validatorHash . buyValidator

buyScript :: MarketParams -> Plutus.Script
buyScript = Ledger.unValidatorScript . buyValidator

buyScriptAsShortBs :: MarketParams -> SBS.ShortByteString
buyScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . buyScript

apiBuyScript :: MarketParams -> PlutusScript PlutusScriptV1
apiBuyScript = PlutusScriptSerialised . buyScriptAsShortBs
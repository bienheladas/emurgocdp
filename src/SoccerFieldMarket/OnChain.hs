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

--import qualified Prelude                                as P
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as Scripts
import qualified Plutus.Script.Utils.V2.Scripts as V2UtilsScripts
import qualified Plutus.V2.Ledger.Contexts                       as Contexts
import qualified Plutus.V2.Ledger.Api                            as LedgerApiV2
import qualified Plutus.V1.Ledger.Interval                            as LedgerIntervalV1
--import qualified Plutus.V1.Ledger.Tx                             as LedgerTxV1
import qualified Plutus.V2.Ledger.Tx                             as LedgerTxV2
import qualified Ledger.Value                                    as Value
import qualified Ledger                                          
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Ledger.Ada                                      as Ada
import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import SoccerFieldMarket.Types    (NFTSale(..), SaleAction(..), MarketParams(..))

{-# INLINABLE nftDatum #-}
nftDatum :: LedgerApiV2.TxOut -> (Ledger.DatumHash -> Maybe Ledger.Datum) -> Maybe NFTSale
nftDatum o f = do
    LedgerTxV2.OutputDatumHash dh <- Just (LedgerTxV2.txOutDatum o)
    Ledger.Datum d <- f (dh)
    PlutusTx.fromBuiltinData d

{-# INLINABLE mkBuyValidator #-}
mkBuyValidator :: MarketParams -> NFTSale -> SaleAction -> LedgerApiV2.ScriptContext -> Bool
mkBuyValidator _ nfts r ctx = case r of
    Buy     -> traceIfFalse "Deadline reached" deadlinepassed &&
               traceIfFalse "The token amount buyed is incorrect" (Value.valueOf (Contexts.valuePaidTo txinfo buyer) (nCurrency nfts) (nToken nfts) == 1) &&
               traceIfFalse "The ADA amount payed to seller is incorrect" (checkSellerOut (nRenter nfts) (nPrice nfts))
               
    Close   -> traceIfFalse "The Tx is not signed by the seller" (Contexts.txSignedBy (Contexts.scriptContextTxInfo ctx) (nRenter nfts))

  where
    txinfo :: Contexts.TxInfo
    txinfo = Contexts.scriptContextTxInfo ctx

    buyer :: Ledger.PubKeyHash
    buyer = case Contexts.txInfoSignatories txinfo of
            [pubKeyHash] -> pubKeyHash

    --checkSingleBuy :: Bool
    --checkSingleBuy = let is = [ i | i <- map txInInfoResolved (txInfoInputs txinfo), txOutAddress i == Address (ScriptCredential $ ownHash ctx) Nothing ] in
    --    length is == 1

    checkSellerOut :: Ledger.PubKeyHash -> Integer -> Bool
    checkSellerOut seller price = fromInteger (Ada.getLovelace (Ada.fromValue (Contexts.valuePaidTo txinfo seller))) >= fromInteger price

    deadlinepassed :: Bool 
    deadlinepassed = LedgerIntervalV1.contains (LedgerIntervalV1.to (nDeadline nfts)) (Contexts.txInfoValidRange txinfo)

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

buyScript :: MarketParams -> LedgerApiV2.Script
buyScript = Ledger.unValidatorScript . buyValidator

buyScriptAsShortBs :: MarketParams -> SBS.ShortByteString
buyScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . buyScript

apiBuyScript :: MarketParams -> PlutusScript PlutusScriptV1
apiBuyScript = PlutusScriptSerialised . buyScriptAsShortBs
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module SoccerFieldMarket.Types
    ( SaleAction (..)
    , SaleSchema
    , StartParams (..)
    , BuyParams (..)
    , NFTSale (..)
    , MarketParams (..)
    )
    where

import           Data.Aeson                (ToJSON, FromJSON)
import           GHC.Generics              (Generic)
import           Prelude                   (Show (..))
import qualified Prelude                   as Pr

import           Schema                    (ToSchema)
import qualified PlutusTx
import           PlutusTx.Prelude          as Plutus ( Eq(..), (&&), Integer )
import           Ledger                    ( TokenName, CurrencySymbol, PubKeyHash, ValidatorHash )
import           Plutus.Contract           ( Endpoint, type (.\/) )
import qualified Plutus.V2.Ledger.Api      as LedgerApiV2

data MarketParams = MarketParams
    { feeAddr  :: PubKeyHash
    , feePrct  :: !Plutus.Integer
    } deriving (Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''MarketParams [('MarketParams, 0)]
PlutusTx.makeLift ''MarketParams


data NFTSale = NFTSale
    { nRenter    :: !PubKeyHash
    , nPrice     :: !Plutus.Integer
    , nCurrency  :: !CurrencySymbol
    , nToken     :: !TokenName
    , nSchedule  :: LedgerApiV2.POSIXTimeRange
    , nDeadline  :: LedgerApiV2.POSIXTime
 --   , nRoyAddr   :: !PubKeyHash
 --   , nRoyPrct   :: !Plutus.Integer
    } deriving (Generic, ToJSON, FromJSON)

instance Eq NFTSale where
    {-# INLINABLE (==) #-}
    a == b = (nRenter    a == nRenter    b) &&
             (nPrice     a == nPrice     b) &&
             (nCurrency  a == nCurrency  b) &&
             (nToken     a == nToken     b) &&
             (nSchedule  a == nSchedule  b) &&
--             (nRoyAddr   a == nRoyAddr   b) &&
--             (nRoyPrct   a == nRoyPrct   b)
             (nDeadline  a == nDeadline  b)

PlutusTx.makeIsDataIndexed ''NFTSale [('NFTSale, 0)]
PlutusTx.makeLift ''NFTSale


data SaleAction = Buy | Close
    deriving Show

PlutusTx.makeIsDataIndexed ''SaleAction [('Buy, 0), ('Close, 1)]
PlutusTx.makeLift ''SaleAction


-- Definimos 2 parametros diferentes para los 2 endpoints 'start' y 'buy' con la informacion minima requerida.
{- Para StartParams omitimos al arrendador debido a que nosotros automaticamente ingresamos la direccion de la billetera al correr el endpoint startSale
    
   Para BuyParams omitimos el arrendador y el precio debido que nosotros podemos leer eso en el datum que puede ser obtenido cotejando los valores 
   del datum 'cs', 'tn' y 'schedule' del token vendido -}

data BuyParams = BuyParams
    { bCs :: CurrencySymbol
    , bTn :: TokenName
    , bSchedule :: LedgerApiV2.POSIXTimeRange
    } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)


data StartParams = StartParams
    { sPrice :: Integer
    , sCs    :: CurrencySymbol
    , sTn    :: TokenName
    , sSchedule :: LedgerApiV2.POSIXTimeRange
    , sDeadline :: LedgerApiV2.POSIXTime
    } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)


type SaleSchema = Endpoint "close" BuyParams
                  .\/
                  Endpoint "buy" BuyParams
                  .\/
                  Endpoint "buy'" (BuyParams, BuyParams)
                  .\/
                  Endpoint "start" StartParams
                  .\/
                  Endpoint "updateContract" ValidatorHash
                  .\/
                  Endpoint "sendToken" Integer

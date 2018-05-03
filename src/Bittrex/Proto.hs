{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Bittrex.Proto where
import           Control.Concurrent (forkIO)
import           Control.Monad (mfilter, forever, unless, void)
import           Data.Text (Text, pack)
import           GHC.Generics
import           Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData, defaultConnectionOptions)
import           Wuss

import           Network.URL
import           Data.Aeson.Encode.Pretty
import qualified Data.Aeson as A
import           Data.Aeson.Types
import qualified Data.Attoparsec.ByteString as ABS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Scientific
import           Data.Binary
import           Data.Binary.Get (ByteOffset)
import           System.IO
import           Control.Monad
import           Data.Time.Clock
import           Util (asRational)
import           Model.MarketModel
{-- Great aeson guide https://artyom.me/aeson --}

-- | Sum type of all possible messages from server we can receive (not complete yet)
data SMsg = SMsg SMessage | SMsgR SMessageResult
  deriving (Generic, Show)

-- | Message from server in form of broadcasted message
data SMessage = SMessage {
    s :: Maybe Integer
  , m :: Maybe Value
  , c :: Maybe Text
} deriving (Generic, Show)

-- | Message from server as response to call
data SMessageResult = SMessageResult {
    r :: Maybe Value
  , i :: Maybe String
} deriving (Generic, Show)

-- JSON parsers for above types. TODO: Rewrite SMsg parser using asum or <|> alternative parser} 
instance FromJSON SMsg where
  parseJSON = withObject "SMsg" $ \v ->
    do maybeR <- v .:? "R" :: Parser (Maybe Value)
       case maybeR of
         Nothing -> do smessage <- parseJSON $ (Object v)
                       return $ SMsg smessage
         Just r -> do smessageResult <- parseJSON $ (Object v)
                      return $ SMsgR smessageResult

instance FromJSON SMessageResult where
  parseJSON = withObject "SMessageResult" $ \v -> SMessageResult
    <$> v .:? "R"
    <*> v .:? "I"
 
instance FromJSON SMessage where
  parseJSON = withObject "SMessage" $ \v -> SMessage
    <$> v .:? "S"
    <*> v .:? "M"
    <*> v .:? "C"
 
    

data ClientSideMsg = ClientSideMsg {
    hubName :: Text
  , methodName :: Text
  , args :: Value
} deriving (Generic, Show)

data ServerSideMsg = ServerSideMsg {
  srvSideMsgHub :: Text
  ,srvSideMsgMethod :: Text
  ,srvSideMsgArgs :: Value
  ,srvSideMsgInvIdent :: String
} deriving (Generic, Show)

instance ToJSON ServerSideMsg where
  toJSON m = object ["H" .= (srvSideMsgHub m), "M" .= (srvSideMsgMethod m), "A" .= (srvSideMsgArgs m), "I" .= (srvSideMsgInvIdent m)]

   
instance FromJSON ClientSideMsg where
  parseJSON = withObject "ClientSideMsg" $ \v -> ClientSideMsg
    <$> v .: "H"
    <*> v .: "M"
    <*> v .: "A"
    

data BtrOrderD = BtrOrderD {
   btrOrdDQuantity :: Rational
 , btrOrdDRate :: Rational
 , btrOrdDType :: Int
} deriving (Eq, Generic, Show)

instance FromJSON BtrOrderD where
  parseJSON = withObject "BtrOrderD" $ \v -> BtrOrderD
    <$> asRational (v .: "Quantity")
    <*> asRational (v .: "Rate")
    <*> v .: "Type"

data BtrFillD = BtrFillD {
   btrFillDQuantity :: Rational
 , btrFillDOrderType :: Text
 , btrFillDOrderRate :: Rational
 , btrFillDOrderTimestamp :: Text
} deriving (Eq, Generic, Show)


instance FromJSON BtrFillD where
  parseJSON = withObject "BtrFillD" $ \v -> BtrFillD
    <$> asRational (v .: "Quantity")
    <*> v .: "OrderType"
    <*> asRational (v .: "Rate")
    <*> v .: "TimeStamp"

data BtrDelta = BtrDelta {
   btrDeltaNounce :: Int
 , btrDeltaMarketName :: Text
 , btrDeltaSells :: [BtrOrderD]
 , btrDeltaFills :: [BtrFillD]
 , btrDeltaBuys :: [BtrOrderD]
} deriving (Generic, Show)


instance FromJSON BtrDelta where
  parseJSON = withObject "BtrDelta" $ \v -> BtrDelta
    <$> v .: "Nounce"
    <*> v .: "MarketName"
    <*> v .: "Sells"
    <*> v .: "Fills"
    <*> v .: "Buys"
  
instance FromJSON BtrOrder where
  parseJSON = withObject "BtrOrder" $ \v -> BtrOrder
    <$> asRational (v .: "Quantity")
    <*> asRational (v .: "Rate")

data BtrFill = BtrFill {
   btrFillType :: Text
 , btrFillQuantity :: Rational
 , btrFillOrderType :: Text
 , btrFillTotal :: Rational
 , btrFillPrice :: Rational
 , btrFillId :: Integer
 , btrFillOrderTimestamp :: Text
} deriving (Eq, Generic, Show)

instance FromJSON BtrFill where
  parseJSON = withObject "BtrFill" $ \v -> BtrFill
    <$> v .: "FillType"
    <*> asRational (v .: "Quantity")
    <*> v .: "OrderType"
    <*> asRational (v .: "Total")
    <*> asRational (v .: "Price")
    <*> v .: "Id"
    <*> v .: "TimeStamp"

data BtrState = BtrState {
    btrStateNounce :: Int
  , btrStateSells :: [BtrOrder]
  , btrStateFills :: [BtrFill]
  , btrStateBuys :: [BtrOrder]
} deriving (Eq, Generic, Show)

instance FromJSON BtrState where
  parseJSON = withObject "BtrState" $ \v -> BtrState
    <$> v .: "Nounce"
    <*> v .: "Sells"
    <*> v .: "Fills"
    <*> v .: "Buys"
 

data BtrMsg = BtrMsgState String BtrState | BtrMsgDelta BtrDelta deriving (Generic, Show)
instance Binary BtrOrder
instance Binary BtrFill
instance Binary BtrOrderD
instance Binary BtrFillD
instance Binary BtrState
instance Binary BtrDelta
instance Binary BtrMsg

data TimedMsg = TimedMsg Integer BtrMsg deriving (Generic, Show)
instance Binary TimedMsg

{-- Convert to MarketModel functions --}
updateOrder :: BtrOrderD -> MarketOrders -> MarketOrders
updateOrder delta market
  | t == 0 = addOrder o market
  | t == 1 = deleteOrder o market
  | t == 2 = replaceOrder o market 
  where o = BtrOrder (btrOrdDQuantity delta) (btrOrdDRate delta)
        t = btrOrdDType delta

updateMarketOrder :: BtrOrderType -> BtrOrderD -> Market -> Market
updateMarketOrder t o m
  | t == Ask = m { asks = updateOrder o (asks m)}
  | t == Bid = m { bids = updateOrder o (bids m)}



showDataLog = 
  withFile "data.log" ReadMode $ \handle -> do
    content <- BL.hGetContents handle
    let msgs = decodeR content
    foldM (\n (TimedMsg ts m) -> do
              let nn = nounce m
              when ((n + 1) /= nn) $
                putStrLn $ "Lost nounce sync at " ++ (show n) ++ " to " ++ (show nn)
              return nn) 0 msgs

    where nounce (BtrMsgState _ s) = btrStateNounce s
          nounce (BtrMsgDelta d) = btrDeltaNounce d

decodeR :: BL.ByteString -> [TimedMsg]
decodeR c
  | BL.null c = []
  | otherwise = let parsed = decodeOrFail c :: Either (BL.ByteString, ByteOffset, String) (BL.ByteString, ByteOffset, TimedMsg)
  in case parsed of
       Left (rest, offset, err) -> error $ ("Error when decoding input: " ++ err)
       Right (rest, offset, msg) -> msg : decodeR rest

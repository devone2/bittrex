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

{-- Great aeson guide https://artyom.me/aeson --}

-- | Sum type of all possible messages from server we can receive (not complete yet)
data SMsg = SMsg SMessage | SMsgR SMessageResult
  deriving (Generic, Show)

-- | Message from in form of broadcasted message
data SMessage = SMessage {
    s :: Maybe Integer
  , m :: Maybe Value
  , c :: Maybe Text
} deriving (Generic, Show)

-- | Message from server as response to call
data SMessageResult = SMessageResult {
    r :: Maybe String
  , i :: Maybe Integer
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
} deriving (Generic, Show)

instance ToJSON ServerSideMsg where
  toJSON m = object ["H" .= (srvSideMsgHub m), "M" .= (srvSideMsgMethod m), "A" .= (srvSideMsgArgs m)]

   
instance FromJSON ClientSideMsg where
  parseJSON = withObject "ClientSideMsg" $ \v -> ClientSideMsg
    <$> v .: "H"
    <*> v .: "M"
    <*> v .: "A"
    

data BtrOrder = BtrOrder {
   btrOrdQuantity :: Double
 , btrOrdRate :: Double
 , btrOrdType :: Int
} deriving (Generic, Show)


instance FromJSON BtrOrder where
  parseJSON = withObject "BtrOrder" $ \v -> BtrOrder
    <$> v .: "Quantity"
    <*> v .: "Rate"
    <*> v .: "Type"

data BtrFill = BtrFill {
   btrFillQuantity :: Double
 , btrFillOrderType :: Text
 , btrFillOrderRate :: Double
 , btrFillOrderTimestamp :: Text
} deriving (Generic, Show)


instance FromJSON BtrFill where
  parseJSON = withObject "BtrFill" $ \v -> BtrFill
    <$> v .: "Quantity"
    <*> v .: "OrderType"
    <*> v .: "Rate"
    <*> v .: "TimeStamp"

data BtrDelta = BtrDelta {
   btrDeltaNounce :: Int
 , btrDeltaMarketName :: Text
 , btrDeltaSells :: [BtrOrder]
 , btrDeltaFills :: [BtrFill]
 , btrDeltaBuys :: [BtrOrder]
} deriving (Generic, Show)


instance FromJSON BtrDelta where
  parseJSON = withObject "BtrDelta" $ \v -> BtrDelta
    <$> v .: "Nounce"
    <*> v .: "MarketName"
    <*> v .: "Sells"
    <*> v .: "Fills"
    <*> v .: "Buys"


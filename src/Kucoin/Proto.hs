{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Kucoin.Proto where

import           Data.Text (Text)
import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple
import           Data.Vector ((!))
import           Data.Aeson.Types
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import           GHC.Generics
import           Util (asRational)
import           Model.MarketModel
--import           Bittrex.Proto
import           Control.Monad (forever)
import           Text.Printf
import           Control.Concurrent
import qualified Data.Map as M
import           Control.Concurrent.Chan

data KuOrderResp = KuOrderResp {
    kuSuccess :: Bool
  , kuCode :: Text
  , kuMsg :: Text
  , kuTimestamp :: Integer
  , kuData :: KuOrders
} deriving (Show, Generic)


instance ToJSON KuOrderResp

instance FromJSON KuOrderResp where
  parseJSON = withObject "KuOrderResp" $ \o -> KuOrderResp
    <$> o .: "success"
    <*> o .: "code"
    <*> o .: "msg"
    <*> o .: "timestamp"
    <*> o .: "data"

data KuOrders = KuOrders {
    kuSells :: [KuOrder]
  , kuBuys :: [KuOrder]
} deriving (Show, Generic)

instance ToJSON KuOrders
instance FromJSON KuOrders where
  parseJSON = withObject "KuOrders" $ \o -> KuOrders
    <$> o .: "SELL"
    <*> o .: "BUY"

instance ToJSON KuOrder
data KuOrder = KuOrder {
    kuRate :: Rational
  , kuQuantity :: Rational
} deriving (Show, Generic)


instance FromJSON KuOrder where
  parseJSON = withArray "KuOrder" $ \a -> KuOrder
    <$> (asRationalParser $ a ! 0)
    <*> (asRationalParser $ a ! 1)
    where asRationalParser = asRational . parseJSON 

downloadBook :: ExPair -> IO (Either String Market)
downloadBook pair = do
  let url = ("https://api.kucoin.com/v1/open/orders?symbol=" ++ (toKuCoinPair pair)) :: String
  req <- parseRequest url
  response <- httpJSON req
  let val = (getResponseBody response) :: Value
  let r = fromJSON val :: Result KuOrderResp
  case r of
    (Success kor) ->  return $ Right $ toMarket kor
    (Error e) -> return $ Left $ show e
  where toKuCoinPair (fx, base) = fx ++ "-" ++ base

toMarket :: KuOrderResp -> Market
toMarket r = let  orders = kuData r
                  m = emptyMarket "ETH-BTC"
                  asksM = foldl (\m' o -> addMarketOrder Ask (toBtrOrder o) m') m (kuSells orders)
             in foldl (\m' o -> addMarketOrder Bid (toBtrOrder o) m') asksM (kuBuys orders)

watchOrderBookSimple = do
  c <- newChan
  watchOrderBook c
  forever $ do
    (Exchange exName exMap) <- readChan c
    putStrLn $ "Got update for exchange = " ++ (show exName) ++ " with number of pairs: " ++ (show $ length $ M.keys exMap)
    
    

watchOrderBook chan = do
  lock <- newMVar ()
  exMap <- newMVar M.empty
  mapM_ (\p -> forkIO $ do
              --atomicPutStrLn lock ("Thread for polling " ++ (show p ) ++ " started")
              forever $ do
                downloadAndShow lock exMap p
                --atomicPutStrLn lock ("Pair " ++ (show p) ++ " going to sleep...")
                threadDelay $ 550 * 1000
          ) (kuPairs)
  

  where formatNumber Nothing = "nothing"
        formatNumber (Just n) = printf "%.8F" (fromRational n :: Double)
        downloadAndShow lock exMapMVar pair = do
          eitherM <- downloadBook pair
          case eitherM of
            Right m -> do 
              modifyMVar_ exMapMVar (\exMap -> do
                                        let newExMap = M.alter (\_ -> Just m) pair exMap
                                        writeChan chan $ Exchange KuCoin newExMap
                                        return newExMap)
                
              --let (ask, bid) = getAskBid m
              --atomicPutStrLn lock $ "Pair: " ++  (show pair) ++  " Bid/Ask= " ++ (formatNumber bid) ++ "/" ++ (formatNumber ask)
            Left e -> putStrLn e

        atomicPutStrLn lock str = withMVar lock (\_ -> putStrLn str)


kuPairs = map (\c -> (c,"BTC")) btcCoins

toBtrOrder :: KuOrder -> BtrOrder
toBtrOrder o = BtrOrder (kuQuantity o) (kuRate o)



btcCoins = ["ETH", "NEO", "FOTA", "CS", "ETN", "GLA", "HAV", "IHT", "KICK", "TIME", "DEB", "ELEC", "LOOM", "LYM", "ONT", "SPHTX", "TRAC", "CHSB", "COV", "DADI", "ELF", "STK", "ZIL", "ZPT", "BPT", "CAPP", "POLY", "TKY", "TNC", "XRB", "AXP", "COFI", "CXO", "DTA", "ING", "KCS", "MTN", "PARETO", "SNC", "TEL", "WAX", "ADB", "BOS", "HAT", "HKN", "HPB", "IOST", "ARY", "DBC", "KEY", "GAT", "RPX", "ACAT", "CV", "DRGN", "LTC", "QLC", "R", "TIO", "CTR", "ITC", "EXY", "MWAT", "AGI", "DENT", "J8T", "LOCI", "OCN", "CAT", "ACT", "ARN", "BCH", "CAN", "EOS", "ETC", "GAS", "JNT", "MEE", "PLAY", "DASH", "DNA", "EBTC", "PRL", "UTK", "CAG", "SPF", "BNTY", "ELIX", "ENJ", "AIX", "VEN", "AION", "DAT", "QTUM", "WTC", "DGB", "SNOV", "AMB", "BTM", "RHOC", "XLR", "XAS", "POE", "UKG", "POLL", "FLIXX", "INS", "OMG", "TFL", "LEND", "KNC", "BCD", "EVX", "LA", "ONION", "POWR", "SNM", "BTG", "HSR", "PBL", "MOD", "PPT", "BCPT", "GVT", "HST", "PURA", "SNT", "SUB", "NEBL", "CVC", "MTH", "STX", "NULS", "PAY", "RDN", "REQ", "QSP", "BHC", "SAY"]




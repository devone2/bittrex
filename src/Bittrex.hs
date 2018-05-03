{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Bittrex where
import           Bittrex.Proto
import qualified Kucoin.Proto as KC
import qualified Bittrex.Watcher as BTR
import           Model.MarketModel
import           Control.Concurrent
import           Control.Monad (mfilter, forever, unless, void, join)
import           Control.Monad as CM
import           Data.Text (Text, pack)
import           GHC.Generics
import           Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData, defaultConnectionOptions)
import           Wuss
import           Data.IORef
import           Network.URL
import           Data.Aeson.Encode.Pretty
import qualified Data.Aeson as A
import           Data.Aeson.Types
import qualified Data.Attoparsec.ByteString as ABS
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe
import qualified Data.Text as T
import           Text.Printf
import Control.Monad
import Control.Concurrent.MVar
import qualified Debug.Trace as D
import           System.IO
import           Data.Binary (encode)
import           Data.Time.Clock.POSIX
import qualified Data.Map as M
import           Data.List (intersect, union)
{-
TODO:
1. import receiving data for multiple markets at the same time
   - maintain map with markets
   - maintain map with invocation to market
   - update correct market

2. calculate possible arb for all markets X for this: (BTC->ETH->X->BTC)

-}

listOfCommonCoins = BTR.marketList `intersect` KC.kuPairs

main :: IO ()
main = do
  putStrLn $ "Starting Arb watcher"

  c <- newChan
  putStrLn $ "Watching Bittrex..."
  forkIO $ BTR.watchOrderBook c

  putStrLn $ "Watching KuCoin..."
  forkIO $ KC.watchOrderBook c

  exchsMVar <- newIORef $ M.empty 
  forever $ do
    ex <- readChan c
    let (Exchange exName exMap) = ex
    modifyIORef exchsMVar (M.alter (\_ -> Just ex) exName)
    exs <- readIORef exchsMVar
    let bittrex = Bittrex `M.lookup` exs
        kucoin = KuCoin `M.lookup` exs
        arbs1 = liftM2 calcArbs bittrex kucoin
        arbs2 = liftM2 calcArbs kucoin bittrex
          
    putStrLn $ "List of arbs from Bittrex -> Kucoin: "
    showArbs arbs1
    putStrLn $ "List of arbs from Kucoin -> Bittrex: "
    showArbs arbs2
    
--    putStrLn $ "Got update for exchange = " ++ (show exName) ++ " with number of pairs: " ++ (show $ length $ M.keys exMap)
    

  putStrLn "Finished"

showArbs (Just arbs) = mapM_ (\(p, ratioMaybe) -> putStrLn $ show p ++ " -> " ++ (formatNumber ratioMaybe)) $ filter (\(_, _) -> True) arbs
showArbs _ = return ()

calcArbs :: Exchange -> Exchange -> [(ExPair, Maybe Rational)]
calcArbs (Exchange _ map1) (Exchange _ map2) =
  let ps = M.keys map1 `intersect` M.keys map2
  in map pairArb ps
  where pairArb p =
          let s1 = getAskBidMarket p map1
              s2 = getAskBidMarket p map2
          in (p, liftM2 arbF2 s1 s2)
        getAskBidMarket pair m = CM.join $ maybeTuple . getAskBid <$> (pair `M.lookup` m)
        arbF2 (a1,_) (_,b2) = b2 / a1


--showArb :: MarketMap -> IO ()
showArb marketsMap = do
  mapM_ (\m -> do
            let (arb1, arb2) = calcArb marketsMap (mkMarket "BTC" m) (mkMarket "ETH" m)
            when (overOne (arb1, arb2)) (putStrLn $ m ++  " Arb = (" ++ (fmt arb1) ++ ", " ++ (fmt arb2) ++ ")")
        ) BTR.coins
  where fmt Nothing = "nothing"
        fmt (Just n) = printf "%.8F" (fromRational n :: Double)
        mkMarket base coin = base ++"-" ++ coin
        overOne (Just a, Just b) = a > 1.003 || b > 1.003
        overOne (_, _) = False
        
data ArbType = Arb1 | Arb2 deriving (Eq, Show)
arbF t (a1,b1) (a2,b2) (a3,b3)
  | t == Arb1 = b1 / (a3 * a2)
  | t == Arb2 = b2 * b3 / a1

--calcArb :: MarketMap -> String -> String -> (Maybe Rational, Maybe Rational)
calcArb m p1 p2 = do
  let s1 = getAskBidMarket p1 --"BTC-NEO"
      s2 = getAskBidMarket p2 --"ETH-NEO"
      s3 = getAskBidMarket "BTC-ETH"
      arb1 = liftM3 (arbF Arb1) s1 s2 s3
      arb2 = liftM3 (arbF Arb2) s1 s2 s3
    in (arb1, arb2)
  where getAskBidMarket name = CM.join $ maybeTuple . getAskBid . snd <$> (name `M.lookup` m)

maybeTuple (Just a, Just b) = Just (a,b)
maybeTuple (_, _) = Nothing

showMarket m = let (minAsk, maxBid) = getAskBid m
               in "Bid = " ++ (formatNumber maxBid) ++ "\n" ++
                  "Ask = " ++ (formatNumber minAsk) ++ "\n" ++
                  "Spr = " ++ (formatNumber ((-) <$> minAsk <*> maxBid))

formatNumber Nothing = "nothing"
formatNumber (Just n) = printf "%.8F" (fromRational n :: Double)


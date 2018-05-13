{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Bittrex.Indicator where
import           Bittrex.Proto
import           Bittrex.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as CC
import           Data.Time.Clock (UTCTime)
import           Control.Concurrent.Chan
import           Conduit
import           Control.Monad (mfilter, forever, unless, void, join)
import           Control.Concurrent (forkIO)
import           Data.Maybe (isJust, fromJust)
import qualified System.IO as IO
import           Data.Binary
mainSimpleI = do
  let marketList = [("ETH", "BTC")] --,("BCC", "BTC"),("ETH", "BTC")]

  c <- newChan
  forkIO $ forever $ watchOrderBook marketList c

  runConduit $ sourceChan c
    .| CC.map extractPrice
    .| CC.filter isJust
    .| CC.map fromJust
    .| CC.scanl (bufferTickers 60) []
    .| CL.mapM_ (displayTickerList)
 


bufferTickers n xs x = take n (x:xs)


displayTickerList [] = putStrLn "No data"
displayTickerList a@(x:xs) = do
  displayTicker x
  putStrLn $ "Avg price: " ++ show (average $ map price a)


price (Ticker t p) = p

average xs = sum xs / fromIntegral (length xs)


downloader = do
  let marketList = [("ETH", "BTC"),("BCC", "BTC"),("ETH", "BTC")]

  c <- newChan
  forkIO $ forever $ watchOrderBook marketList c

  runConduit $ sourceChan c
    .| CC.map encode
    .| CC.sinkIOHandle (IO.openBinaryFile "data2.log" IO.AppendMode)

    .| CC.map fromJust
    .| CC.scanl (bufferTickers 60) []
    .| CL.mapM_ (displayTickerList)
 

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Bittrex where
import           Bittrex.Proto
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

main :: IO ()
main = do
  putStrLn $ "Relative url: " ++ (exportURL relativeUrl)
  let options = defaultConnectionOptions
  let headers = [("Cookie","__cfduid=d11e6a51d6d37fa3926599c6a50a090191511988745; cf_clearance=e94417b5a9746498f99ad11a1994311528ab10e3-1519465540-10800")
                ,("User-Agent","Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/64.0.3282.167 Safari/537.36")
               ] 
  runSecureClientWith "socket.bittrex.com"  443 (exportURL relativeUrl) options headers ws
  
  putStrLn "Finished"
  putStrLn $ exportURL relativeUrl 

relativeUrl = URL HostRelative "signalr/connect" socketParams

socketParams =
  [ ("transport", "webSockets")
  , ("clientProtocol", "1.5")
  , ("connectionToken","yhHkOduf8S+BP8KzWIz/JaJmPxBvdf1BrEZz5O1uxU0WlAbmvgqerHHm22mmPMSWIzwKVBNd5sU7QUC78ohyM0Er5xsQPHzrvFtUH/ZSz2XFAMeK")
  , ("connectionData","[{\"name\":\"corehub\"}]")
  , ("tid","1")
  ]



subscribeToExchangeDeltas pair = ServerSideMsg "corehub" "SubscribeToExchangeDeltas" (toJSON [pair])

parseClientSideMsg smsg = (m smsg) >>= parseMaybe parseJSON  :: Maybe [ClientSideMsg]

parseDelta csm = parseEither parseJSON (args csm) :: Either String [BtrDelta]

  
show2 smsg = encodePretty $ m smsg

show3 :: ClientSideMsg -> BL.ByteString
show3 csm = encodePretty $ args csm



ws :: ClientApp ()
ws connection = do
    putStrLn "Connected!"
    putStrLn "Requesting exchangeDeltas"
    let q = A.encode $ subscribeToExchangeDeltas ("BTC-ETH"::String)
    BL.putStrLn q
    sendTextData connection q

    void . forkIO . forever $ do
        message <- (receiveData connection) :: IO BL.ByteString
        putStrLn "------- Message Received ----------"
--        BL.putStr "Full msg:"
--        BL.putStrLn message
        let parsedMsg = A.eitherDecode message :: Either String SMessage
        case parsedMsg of
          Left msg -> error $ "Problem parsing: " ++ msg
          Right parsedResult -> do
            let maybeClientSideMsgs = (parseClientSideMsg parsedResult)
            let r = fmap (\z -> mfilter (\x -> (methodName x) /= "updateSummaryState") z) maybeClientSideMsgs
            case r of
              Nothing -> putStrLn $ "Unknown message" ++ (show message)
              Just xs -> do
                BL.putStrLn $ BL.take 1500 $ BL.concat $ map show3 xs
                let deltas = map parseDelta xs
                putStrLn $ take 1500 $ concat $ map show deltas 
  
        putStrLn "------- Message End ---------------"
    let loop = do
            line <- getLine
            unless (null line) $ do
                sendTextData connection (pack line)
                loop
    loop

    sendClose connection (pack "Bye!")

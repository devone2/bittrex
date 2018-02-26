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
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Maybe
import qualified Data.Text as T
main :: IO ()
main = do
  putStrLn $ "Relative url: " ++ (exportURL relativeUrl)
  cf <- readCloudFlare
  putStrLn $ "Using cf:" ++ (show cf)
  let options = defaultConnectionOptions
  let headers = [("Cookie", BS.pack (cookies cf))
                ,("User-Agent", BS.pack (userAgent cf))] 
  runSecureClientWith "socket.bittrex.com" 443 (exportURL relativeUrl) options headers ws
  
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

readCloudFlare = do
  content <- BL.readFile "cf.json"
  let eitherR = A.eitherDecode content :: Either String CloudFlare
  case eitherR of
    Left msg -> error msg
    Right cf -> return cf


data CloudFlare = CloudFlare {
    cookies :: String
  , userAgent :: String
} deriving (Generic, Show)

instance FromJSON CloudFlare




subscribeToExchangeDeltas pair = ServerSideMsg "corehub" "SubscribeToExchangeDeltas" (toJSON [pair])
queryExchangeState pair = ServerSideMsg "corehub" "QueryExchangeState" (toJSON [pair])


 
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
    let q = A.encode $ subscribeToExchangeDeltas ("BTC-RDD"::String)
    BL.putStrLn q
    sendTextData connection q


    let queryCommand = A.encode $ queryExchangeState ("BTC-ETH"::String)
    BL.putStrLn queryCommand
    sendTextData connection queryCommand
    
    void . forkIO . forever $ do
        message <- (receiveData connection) :: IO BL.ByteString
--        putStrLn "------- Message Received ----------"
        let eitherBtrMsg = (decodeToSMsg message) >>= decodeSMsg
        case eitherBtrMsg of
          Left err -> putStrLn $ "Failed to decode message from server" ++ err
          Right btrMsg ->
            case btrMsg of
              BtrMsgState state -> putStrLn $ "State msg: n=" ++ (show $ btrStateNounce state)
              BtrMsgDelta delta -> putStrLn $ "Delta msg: n=" ++ (show $ btrDeltaNounce delta) ++ " market = " ++ (T.unpack $ btrDeltaMarketName delta)
--        putStrLn "------- Message End ---------------"
    let loop = do
            line <- getLine
            unless (null line) $ do
                sendTextData connection (pack line)
                loop
    loop

    sendClose connection (pack "Bye!")


parseClientSideMsg :: SMessage -> Maybe [ClientSideMsg]
parseClientSideMsg smsg = (m smsg) >>= parseMaybe parseJSON   

parseDelta :: ClientSideMsg -> Either String [BtrDelta]
parseDelta csm
  | method == "updateExchangeState" = parseEither parseJSON (args csm)
  | otherwise = Left $ "Unknown methodName " ++ (T.unpack method) ++ ". Expected updateExchangeState for incoming Delta"
  where method = methodName csm


parseState :: SMessageResult -> Either String BtrState
parseState smr = parseEither parseJSON (fromJust $ r smr)

decodeToSMsg :: BL.ByteString -> Either String SMsg  
decodeToSMsg rawMessage = A.eitherDecode rawMessage 

-- | Function to decode SMsg. Be carefull may lose some messages because it takes only first element od response array. Because so far I havent sean bigger arrays.
decodeSMsg :: SMsg -> Either String BtrMsg
decodeSMsg smsg = case smsg of
  SMsg smessage ->
    let maybeClientSideMsg = (parseClientSideMsg smessage) >>= listToMaybe
        in case maybeClientSideMsg of
           Nothing -> Left $ "Failed to decode smessage:" ++ (show smessage)
           Just csm -> case parseDelta csm of
                         Left err -> Left $ "Failed decoding delta from clientSideMsg: " ++ err
                         Right (x:xs) -> Right $ BtrMsgDelta x
  SMsgR smr ->
    let state = parseState smr
        in fmap BtrMsgState state

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Bittrex.Conduit where
import           Bittrex.Proto
import           Model.MarketModel
import           Control.Concurrent (forkIO)
import           Control.Monad (mfilter, forever, unless, void, join)
import           Control.Monad as CM
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
import           Text.Printf
import Control.Monad
import Control.Concurrent.MVar
--import           Control.Concurrent
import qualified Debug.Trace as D
import           System.IO
import           Data.Binary (encode)
import           Data.Time.Clock.POSIX
import qualified Data.Map as M
import           Data.List (intersect, union)
import           Control.Concurrent.Chan
import           Conduit
import qualified Data.Conduit.List as CL
import           Data.IORef

{-
TODO:
1. import receiving data for multiple markets at the same time
   - maintain map with markets
   - maintain map with invocation to market
   - update correct market
-}

mainSimple = do
  let marketList = [("ETH", "BTC")] --,("BCC", "BTC"),("ETH", "BTC")]

  c <- newChan
  forkIO $ forever $ watchOrderBook marketList c

  runConduit $ sourceChan c
    .| CL.mapM_ displayPrice
  
--  forever $ do
--    tm@(TimedMsg t msg) <- readChan c
--    let xmr_btc = M.
--    putStrLn $ "OrderBook: " ++ 
--    putStrLn $ "[" ++ (show $ epochToUTC t) ++ "] msg: " ++ (take 20 $ show msg)
--    displayPrice tm

displayPrice (TimedMsg t msg) = 
  case msg of
      BtrMsgDelta delta -> do
        let fills = btrDeltaFills delta
        when (not $ null fills) $ do
          putStrLn $ "[" ++ (show $ epochToUTC t) ++ "] price: " ++ (fmtMoney $ safeHead $ map btrFillDOrderRate fills)
          
      BtrMsgState i state -> do
        let fills = btrStateFills state
        when (not $ null fills) $ do
          putStrLn $ "[" ++ (show $ epochToUTC t) ++ "] price: " ++ (fmtMoney $ safeHead $ map btrFillPrice fills)
 

sourceChan :: Chan a -> ConduitT () a IO ()
sourceChan chan = loop
  where loop = do
          value <- lift $ readChan chan
          yield value
          loop


safeHead [] = Nothing
safeHead (x:xs) = Just x
fmtMoney Nothing = "nothing"
fmtMoney (Just n) = printf "%.8F" $ (fromRational n :: Double)

epochToUTC = posixSecondsToUTCTime . fromIntegral
watchOrderBook marketList chan = do
  putStrLn $ "Relative url: " ++ (exportURL relativeUrl)
  cf <- readCloudFlare
  putStrLn $ "Using cf:" ++ (show cf)
  let options = defaultConnectionOptions
  let headers = [("Cookie", BS.pack (cookies cf))
                ,("User-Agent", BS.pack (userAgent cf))] 
  runSecureClientWith "socket.bittrex.com" 443 (exportURL relativeUrl) options headers $ ws marketList chan
  
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


subscribeToExchangeDeltas pair = ServerSideMsg "corehub" "SubscribeToExchangeDeltas" (toJSON [toBittrexPair pair]) "-1"
queryExchangeState pair = ServerSideMsg "corehub" "QueryExchangeState" (toJSON [pair])

 
--show2 smsg = encodePretty $ m smsg

show3 :: ClientSideMsg -> BL.ByteString
show3 csm = encodePretty $ args csm


queryExState connection marketName i = do
    let queryCommand = A.encode $ queryExchangeState (toBittrexPair marketName) i
    sendTextData connection queryCommand


toBittrexPair (fx,base) = base ++ "-" ++ fx
fromBittrexPair s = let (bs, fxs) = break (=='-') s
                  in (tail fxs, bs)
data BtrControl = BtrControl Integer Integer



ws :: [ExPair] -> Chan TimedMsg -> ClientApp ()
ws marketList chan connection = do
    putStrLn "Connected!"
    putStrLn "Requesting exchangeDeltas"

    -- M.Map String ExPair
    invIORef <- newIORef $ M.empty  :: IO (IORef (M.Map String ExPair))

    -- M.Map ExPair Int - map from pair to last nounce
    nouncesIORef <- newIORef $ M.empty :: IO (IORef (M.Map ExPair Int))

    mapM_ (\m -> do
              let q = A.encode $ subscribeToExchangeDeltas m 
              putStrLn $ "Subscribing to deltas: " ++ (show m)
              sendTextData connection q
          ) marketList 

    forever $ do
        message <- (receiveData connection) :: IO BL.ByteString
        let eitherBtrMsg = (decodeToSMsg message) >>= decodeSMsg
        case eitherBtrMsg of
          Left err -> putStrLn $ "Failed to decode message from server" ++ err
          Right btrMsg -> do
            now <- round `fmap` getPOSIXTime
            case btrMsg of
              BtrMsgState i state -> do
                putStrLn $ show $ map btrFillOrderTimestamp $ btrStateFills state
                invMap <- readIORef invIORef
                let marketName = invMap M.! i
                modifyIORef nouncesIORef (M.alter (\_ -> Just $ btrStateNounce state) marketName)
                writeChan chan $ TimedMsg now btrMsg
              BtrMsgDelta delta -> do
                nounces <- readIORef nouncesIORef
                let currentNounce = btrDeltaNounce delta
                    marketName = marketNameD delta
                    lastNounce = nounces M.!? marketName
                if not ((currentNounce - 1) `elem` lastNounce) then do
                  -- we are out of sync with server -> we will requests full state
                  queryFullState connection invIORef marketName
                else do
                  -- just update last nounce
                  modifyIORef nouncesIORef (M.update (\_ -> Just currentNounce) marketName)
                  writeChan chan $ TimedMsg now btrMsg

--  TODO: vymysliet ako zavriet socket ked skonci thready
--  sendClose connection (pack "Bye!")
    putStrLn "Main thread goind to sleep"

marketNameD :: BtrDelta -> ExPair
marketNameD delta = fromBittrexPair $ T.unpack $ btrDeltaMarketName delta

       
maybeTuple (Just a, Just b) = Just (a,b)
maybeTuple (_, _) = Nothing

queryFullState connection invIORef marketName = do
  invMap <- readIORef invIORef
  let ks = map read (M.keys invMap) :: [Integer]
      i = show $ 1 + (if null ks then 0 else maximum ks)
  modifyIORef invIORef (M.insert i marketName)
  queryExState connection marketName i

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

-- | Function to decode SMsg. Be carefull may lose some messages because it takes only first element of response array. Because so far I havent seen bigger arrays.
decodeSMsg :: SMsg -> Either String BtrMsg
decodeSMsg smsg = case smsg of
  SMsg smessage ->
    let maybeClientSideMsg = (parseClientSideMsg smessage) >>= listToMaybe
        in case maybeClientSideMsg of
           Nothing -> Left $ "Failed to decode smessage:" ++ (show smessage)
           Just csm -> {-D.trace ("Delta original: " ++ (BL.unpack $ show3 csm)) $-} case parseDelta csm of
                         Left err -> Left $ "Failed decoding delta from clientSideMsg: " ++ err
                         Right (x:xs) -> Right $ BtrMsgDelta x
  SMsgR smr ->
    let state = {--D.trace ("SMessageResult: " ++ (show smr)) $--} parseState smr
        in fmap (BtrMsgState $ fromMaybe "-2" (i smr)) state


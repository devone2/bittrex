{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Bittrex.Conduit where
import           Bittrex.Proto
import qualified Bittrex.ProtoUtil as PU
import           Bittrex.Reader
import           Model.MarketModel
import           Control.Concurrent (forkIO)
import           Control.Monad (mfilter, forever, unless, void, join)
import           Control.Monad as CM
import           Data.Text (Text, pack)
import           GHC.Generics
import           Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData, defaultConnectionOptions)
import           Wuss
import           Network.WebSockets (ConnectionException(..))

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
import           Control.Exception (catch)
import qualified Debug.Trace as D
import           System.IO
import           Data.Binary (encode)
import           Data.Time.Clock.POSIX
import qualified Data.Map as M
import           Data.List (intersect, union)
import           Control.Concurrent.Chan
import           Conduit

import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as CC

import           Data.IORef
import           Data.Time.Clock (UTCTime, diffUTCTime)
import qualified Data.Time.Format as TF

import qualified System.IO as IO
import qualified Bittrex.TimeUtil as UTC
import           Data.Time.Format (formatTime, defaultTimeLocale)
{-
TODO:
1. import receiving data for multiple markets at the same time
   - maintain map with markets
   - maintain map with invocation to market
   - update correct market
-}

mainSimple = do
  let marketList = [("ETH", "BTC")] --,("BCC", "BTC"),("ETH", "BTC")]
{--
  c <- newChan
  forkIO $ forever $ watchOrderBook marketList c
--}
{-- 
  runConduit $ sourceChan c
    .| CC.map extractPrice
    .| CC.filter isJust
    .| CL.mapM_ (displayTicker . fromJust)
--}
  runConduit $ dataSource
--    .| CC.mapM_ (\(TimedMsg t m)-> putStrLn $ show $ PU.marketName m)
    .| CC.filter (\(TimedMsg t m)-> PU.marketName m == ("XMR","BTC"))
    .| CC.scanlM collectFills []
    .| CC.map avg60Fills
    .| sinkNull
 --   .| CC.mapM_ (putStrLn . fmtMoney . fromRational)
{--
  total <- runConduit $ dataSource
    .| CC.filter (\(TimedMsg _ m) -> isState m)
    .| CC.mapM_ (putStrLn . stateStats)
--    .| CC.map avg60Fills
--    .| CC.filter (>1)
    .| CC.sum
--    .| CL.mapM_ (putStrLn . fmtMoney . fromRational)
  putStrLn $ "Total number of state msgs:" ++ (show total)
-- CL.mapAccumM
--}
stateStats (TimedMsg t (BtrMsgState s)) =
  let fil = map btrFillOrderTimestamp $ btrStateFills s
      minD = minimum fil
      maxD = maximum fil
      d = diffUTCTime maxD minD
  in concat ["[", show t, "] ", "Fills count=", show $ length fil, ", "," span = ", formatDiff $ floor d]


stateStats _ = "Not a state message"

formatDiff total_s =
  let (d,r1) = total_s `divMod` (3600*24)
      (h,r2) = r1 `divMod` 3600
      (m,s) = r2 `divMod` 60
  in concat [show d, " days ", show h, ":", show m, ":", show s]
isState (BtrMsgState _) = True
isState _ = False

collectFillsT :: Timed [BtrFillD] -> TimedMsg -> IO (Timed [BtrFillD])
collectFillsT (Timed _ acc) m@(TimedMsg t _) = Timed t <$> (collectFills acc m)

collectFills :: [BtrFillD] -> TimedMsg -> IO [BtrFillD]
collectFills fills (TimedMsg t (BtrMsgDelta delta)) = do
--  putStrLn $ "Delta fills: " ++ (show $ btrDeltaFills delta)
  let s = (btrDeltaFills delta) ++ fills
--  putStrLn $ "[" ++ show t ++ "] Got delta update "
  return s
collectFills fills (TimedMsg t (BtrMsgState state)) = do
  let theirFills = btrStateFills state
  
  if not (null fills || null theirFills) then do
    let latest = maximum $ map btrFillDOrderTimestamp fills
    let first = minimum $ map btrFillOrderTimestamp theirFills
  
    let fresh = filter (\x -> btrFillOrderTimestamp x > latest) $ theirFills
    let freshD = map fromFill fresh
    when (not $ null freshD) $ do
      putStrLn $ "[" ++ show t ++ "] " ++ (show $ btrStateMarketName state) ++" state update my latest: " ++ show latest ++ ", theirs first: " ++ show first ++ " Fresh to add: " ++ (show $ length freshD)
    return $ fills ++ freshD
  else
    return $ fills ++ (map fromFill theirFills)

  where fromFill x = BtrFillD (btrFillQuantity x) (btrFillOrderType x) (btrFillPrice x) (btrFillOrderTimestamp x) 

avg60Fills xs =
  let last60 = take 60 xs
      sum60 = sum $ map btrFillDOrderRate last60
      len = length last60
  in if len > 0 then (sum60 / (fromIntegral len)) else 0

mainSimpleBtr = do
  let marketList = [("ETH", "BTC"),("BCC", "BTC"),("XMR", "BTC")]

  c <- newChan
  forkIO $ forever $ watchOrderBook marketList c

  runConduit $ sourceChan c
    .| CL.mapM_ (displayBtr)

mainWriter = do 
  let marketList = [("ETH", "BTC"),("BCC", "BTC"),("XMR", "BTC")]

  c <- newChan
  forkIO $ forever $ watchOrderBook marketList c

  writeLoop c Nothing
  where writeLoop c s = do
          event@(TimedMsg t m) <- readChan c
          (file, nextState) <- getNextState s t
          BL.hPut file (encode event)
          displayBtr event
          writeLoop c $ Just nextState
        getNextState s t = do
          let eventBlockStart = blockStartTime t
          case s of
            Nothing -> do h <- openNewBinary eventBlockStart
                          return (h, WriteState eventBlockStart h)
            Just s -> do h <- if (startTime s == eventBlockStart) then
                                 return $ file s
                              else do
                                 IO.hClose $ file s
                                 openNewBinary eventBlockStart
                         return $ (h, WriteState eventBlockStart h)

        openNewBinary t = IO.openBinaryFile (mkFilename t) IO.AppendMode
        blockStartTime t = UTC.zeroAfterHours t

        mkFilename t = concat ["data/bittrex_", formatTime defaultTimeLocale "%F_%T" t, ".dat"]

data WriteState = WriteState {
  startTime :: UTCTime
 , file :: IO.Handle
}

--  forever $ do
--    tm@(TimedMsg t msg) <- readChan c
--    let xmr_btc = M.
--    putStrLn $ "OrderBook: " ++ 
--    putStrLn $ "[" ++ (show $ epochToUTC t) ++ "] msg: " ++ (take 20 $ show msg)
--    displayPrice tm

data Ticker = Ticker UTCTime Double 

displayTicker (Ticker t p) =
  putStrLn $ "[" ++ (show t) ++ "] price: " ++ (fmtMoney p)


extractPrice (TimedMsg t msg) = 
  case msg of
      BtrMsgDelta delta -> 
        let lastFill = safeHead $ btrDeltaFills delta
        in Ticker <$> (btrFillDOrderTimestamp <$> lastFill) <*> (fromRational <$> btrFillDOrderRate <$> lastFill)
      BtrMsgState state -> 
        let lastFill = safeHead $ btrStateFills state
        in Ticker <$> (btrFillOrderTimestamp <$> lastFill) <*> (fromRational <$> btrFillPrice <$> lastFill)

sourceChan :: Chan a -> ConduitT () a IO ()
sourceChan chan = loop
  where loop = do
          value <- lift $ readChan chan
          yield value
          loop


safeHead [] = Nothing
safeHead (x:xs) = Just x

fmtMoneyM Nothing = "nothing"
fmtMoneyM (Just n) = printf "%.8F" $ (fromRational n :: Double)

fmtMoney = printf "%.8F" 

epochToUTC = posixSecondsToUTCTime . fromIntegral

watchOrderBook marketList chan = do
  goCatched
  putStrLn "Finished"
  putStrLn $ exportURL relativeUrl 

  where go = do
          putStrLn $ "Relative url: " ++ (exportURL relativeUrl)
          cf <- readCloudFlare
          putStrLn $ "Using cf:" ++ (show cf)
          let options = defaultConnectionOptions
          let headers = [("Cookie", BS.pack (cookies cf))
                        ,("User-Agent", BS.pack (userAgent cf))] 
          runSecureClientWith "socket.bittrex.com" 443 (exportURL relativeUrl) options headers $ ws marketList chan
        goCatched = do
          go `catch` (\(e :: ConnectionException) -> do
                        putStrLn $ "ConnectionException closed connection: " ++(show e)++" . Reconnecting..."
                        goCatched)
            
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


subscribeToExchangeDeltas pair = ServerSideMsg "corehub" "SubscribeToExchangeDeltas" (toJSON [ExPairBtr pair]) "-1"
queryExchangeState pair = ServerSideMsg "corehub" "QueryExchangeState" (toJSON [ExPairBtr pair])

 
--show2 smsg = encodePretty $ m smsg

show3 :: ClientSideMsg -> BL.ByteString
show3 csm = encodePretty $ args csm


queryExState connection marketName i = do
    let queryCommand = A.encode $ queryExchangeState marketName i
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
            now <- getCurrentTime
            case btrMsg of
              BtrMsgParsedState i state -> do
                invMap <- readIORef invIORef
                let marketName = invMap M.! i
                modifyIORef nouncesIORef (M.alter (\_ -> Just $ btrStateNounceNM state) marketName)
                writeChan chan $ TimedMsg now $ BtrMsgState $ mkBtrState marketName state
              BtrMsgParsedDelta delta -> do
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
                  writeChan chan $ TimedMsg now $ BtrMsgDelta delta

--  TODO: vymysliet ako zavriet socket ked skonci thready
--  sendClose connection (pack "Bye!")
    putStrLn "Main thread goind to sleep"

marketNameD :: BtrDelta -> ExPair
marketNameD delta = btrDeltaMarketName delta

       
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


parseState :: SMessageResult -> Either String BtrStateNoMarket
parseState smr = parseEither parseJSON (fromJust $ r smr)

decodeToSMsg :: BL.ByteString -> Either String SMsg  
decodeToSMsg rawMessage = A.eitherDecode rawMessage 

-- | Function to decode SMsg. Be carefull may lose some messages because it takes only first element of response array. Because so far I havent seen bigger arrays.
decodeSMsg :: SMsg -> Either String BtrMsgParsed
decodeSMsg smsg = case smsg of
  SMsg smessage ->
    let maybeClientSideMsg = (parseClientSideMsg smessage) >>= listToMaybe
        in case maybeClientSideMsg of
           Nothing -> Left $ "Failed to decode smessage:" ++ (show smessage)
           Just csm -> {-D.trace ("Delta original: " ++ (BL.unpack $ show3 csm)) $-} case parseDelta csm of
                         Left err -> Left $ "Failed decoding delta from clientSideMsg: " ++ err
                         Right (x:xs) -> Right $ BtrMsgParsedDelta x
  SMsgR smr ->
    let state = {--D.trace ("SMessageResult: " ++ (show smr)) $--} parseState smr
        in fmap (BtrMsgParsedState $ fromMaybe "-2" (i smr)) state


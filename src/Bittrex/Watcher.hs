{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Bittrex.Watcher where
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
import           Control.Concurrent
import qualified Debug.Trace as D
import           System.IO
import           Data.Binary (encode)
import           Data.Time.Clock.POSIX
import qualified Data.Map as M
import           Data.List (intersect, union)
import           Control.Concurrent.Chan
{-
TODO:
1. import receiving data for multiple markets at the same time
   - maintain map with markets
   - maintain map with invocation to market
   - update correct market

2. calculate possible arb for all markets X for this: (BTC->ETH->X->BTC)

-}

btcCoins = ["TRX", "XVG", "ADA", "ETH", "XEM", "XRP", "XLM", "NEO", "NBT", "UP", "BCC", "LTC", "LRC", "SC", "TUSD", "SLS", "QTUM", "ETC", "RDD", "XMR", "ADT", "ZEC", "STRAT", "VTC", "SALT", "LSK", "DASH", "BTG", "OMG", "DGB", "RVR", "AMP", "ENG", "SNT", "DOGE", "SRN", "ARK", "NXS", "STEEM", "FTC", "XDN", "PAY", "ZCL", "KORE", "BAT", "ARDR", "VEE", "WAVES", "FCT", "POWR", "VIA", "SYS", "MANA", "BCPT", "GAME", "STORJ", "EMC2", "KMD", "XZC", "MCO", "UNB", "WAX", "NXT", "TX", "EDG", "BAY", "IGNIS", "REP", "ADX", "ZEN", "GBYTE", "BITB", "PIVX", "RLC", "RCN", "HMQ", "DCR", "ION", "GRS", "LBC", "EXP", "SBD", "CVC", "BLOCK", "MONA", "BLK", "MUE", "CFI", "PKB", "ZRX", "XCP", "ANT", "PART", "QRL", "BURST", "DNT", "UKG", "DOPE", "VRC", "GNT", "SWT", "VIB", "NMR", "WINGS", "XEL", "SLR", "SHIFT", "GCR", "THC", "OK", "PDC", "MEME", "CPC", "GUP", "TIX", "UBQ", "CLOAK", "IOP", "SIB", "LUN", "LMC", "NAV", "DYN", "MER", "GOLOS", "FAIR", "SPHR", "NLG", "POT", "GNO", "DCT", "ENRG", "INCNT", "RADS", "PPC", "MUSIC", "XMY", "PTOY", "EMC", "BNT", "EBST", "SEQ", "BSD", "XWC", "EXCL", "CANN", "START", "AEON", "GLD", "GEO", "TRST", "LGD", "CRW", "DMD", "COVAL", "CRB", "GAM", "SYNX", "DTB", "IOC", "NEOS", "XVC", "VRM", "EFL", "XMG", "SWIFT", "AUR", "GRC", "TRUST", "QWARK", "SPR", "FLDC", "EGC", "FLO", "ABY", "BCY", "RBY", "PINK", "BRK", "VTR", "CLAM", "TKS", "OMNI", "BLITZ", "NXC", "MLN", "CURE", "PTC", "XST", "GBG", "BYC", "2GIVE", "BRX", "SNRG", "ERC", "CLUB"]

ethCoins = ["ADA", "BCC", "XEM", "QTUM", "NEO", "LTC", "XRP", "OMG", "XMR", "MCO", "TRX", "WAX", "DASH", "SRN", "ZEC", "SC", "DGB", "XLM", "CVC", "ETC", "REP", "NMR", "PAY", "RLC", "POWR", "STRAT", "GNT", "ZRX", "HMQ", "SNT", "BTG", "CFI", "ADX", "BAT", "TRST", "WAVES", "MANA", "BCPT", "ENG", "WINGS", "LRC", "FCT", "VEE", "DNT", "RCN", "SALT", "TIX", "UKG", "BNT", "STORJ", "QRL", "ANT", "ADT", "PTOY", "GUP", "GNO", "LUN", "CRB", "VIB", "LGD"]

coins = btcCoins `union` ethCoins
--main :: IO ()

mainSimple = do
  c <- newChan
  forkIO $ forever $ watchOrderBook c
  forever $ do
    (Exchange exName exMap) <- readChan c
    putStrLn $ "Got update for exchange = " ++ (show exName) ++ " with number of pairs: " ++ (show $ length $ M.keys exMap)


watchOrderBook chan = do
  putStrLn $ "Relative url: " ++ (exportURL relativeUrl)
  cf <- readCloudFlare
  putStrLn $ "Using cf:" ++ (show cf)
  let options = defaultConnectionOptions
  let headers = [("Cookie", BS.pack (cookies cf))
                ,("User-Agent", BS.pack (userAgent cf))] 
  runSecureClientWith "socket.bittrex.com" 443 (exportURL relativeUrl) options headers $ ws chan
  
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


 
show2 smsg = encodePretty $ m smsg

show3 :: ClientSideMsg -> BL.ByteString
show3 csm = encodePretty $ args csm


loadMarket :: BtrState -> Market
loadMarket s = let m = emptyMarket "BTC-EUR"
                   m1 = foldl (\m' x -> addMarketOrder Ask x m') m (btrStateSells s)
               in foldl (\m' x -> addMarketOrder Bid x m') m1 (btrStateBuys s)

updateMarket :: Market -> BtrDelta -> Market
updateMarket m d = let m1 = foldl (\m' x -> updateMarketOrder Ask x m') m (btrDeltaSells d)
                   in foldl (\m' x -> updateMarketOrder Bid x m') m1 (btrDeltaBuys d)

queryExState connection marketName i = do
    let queryCommand = A.encode $ queryExchangeState (toBittrexPair marketName) i
    --BL.putStrLn queryCommand
    sendTextData connection queryCommand

marketList = [("ETH", "BTC")] `union` (map (\m -> (m, "BTC")) coins) `union` (map (\m -> (m, "ETH")) coins)

toBittrexPair (fx,base) = base ++ "-" ++ fx
fromBittrexPair s = let (bs, fxs) = break (=='-') s
                  in (tail fxs, bs)
data BtrControl = BtrControl Integer Integer


type MarketMap = M.Map String (Int, Market)

ws :: Chan Exchange -> ClientApp ()
ws chan connection = do
    putStrLn "Connected!"
    putStrLn "Requesting exchangeDeltas"

    marketsMVar <- newMVar $ M.fromList $ map (\m ->(m, (0, emptyMarket $ show m))) marketList
    invMVar <- newMVar $ M.empty :: IO (MVar (M.Map String ExPair))

    mapM_ (\m -> do
              let q = A.encode $ subscribeToExchangeDeltas m 
              putStrLn $ "Subscribing to deltas: " ++ (show m)
              sendTextData connection q
          ) marketList 

    void . forkIO . forever $ withFile "data.log" AppendMode $ \handle -> do
        message <- (receiveData connection) :: IO BL.ByteString
        let eitherBtrMsg = (decodeToSMsg message) >>= decodeSMsg
        case eitherBtrMsg of
          Left err -> putStrLn $ "Failed to decode message from server" ++ err
          Right btrMsg -> do
            now <- round `fmap` getPOSIXTime
            BL.hPut handle (encode $ TimedMsg now btrMsg)
            case btrMsg of
              BtrMsgState i state -> do
                updateFullState marketsMVar invMVar i state 
              BtrMsgDelta delta -> do
                updateDeltaState connection marketsMVar invMVar delta
                mts <- readMVar marketsMVar
                writeChan chan $ Exchange Bittrex $ M.map (\(_,m) -> m) mts
--  TODO: vymysliet ako zavriet socket ked skonci thready
--  sendClose connection (pack "Bye!")
    putStrLn "Main thread goind to sleep"
    forever $ threadDelay (1000 * 50)


       
maybeTuple (Just a, Just b) = Just (a,b)
maybeTuple (_, _) = Nothing

updateFullState marketsMVar invMVar i state = do
  invMap <- readMVar invMVar
  let marketName = invMap M.! i
  modifyMVar_ marketsMVar (\marketsMap ->
                              return $ M.alter (\_ -> Just (btrStateNounce state, loadMarket state)) marketName marketsMap)

--updateDeltaState :: a -> MVar (M.Map ExPair (Int,Market)) -> MVar (M.Map String ExPair) -> BtrDelta -> IO ()
updateDeltaState connection marketsMVar invMVar delta = do
  let marketName = fromBittrexPair $ T.unpack $ btrDeltaMarketName delta
  modifyMVar_ marketsMVar (\marketsMap -> do
                              let (lastNounce, market) = (marketsMap M.! marketName)
                              when (lastNounce+1 /= (btrDeltaNounce delta)) $ do
                                queryFullState connection invMVar marketName
                                --putStrLn ("Lost sync with exchange at old nounce = " ++ (show lastNounce))
                              return $ M.update (\_-> Just (btrDeltaNounce delta, updateMarket market delta)) marketName marketsMap)

--queryFullState :: a -> MVar (M.Map String ExPair) -> ExPair -> IO ()
queryFullState connection invMVar marketName = do
  modifyMVar_ invMVar (\invMap -> do 
                          let ks = map read (M.keys invMap) :: [Integer]
                          let i = show $ 1 + (if null ks then 0 else maximum ks)
                          queryExState connection marketName i
                          return $ M.insert i marketName invMap)

showMarket m = let (minAsk, maxBid) = getAskBid m
               in "Bid = " ++ (formatNumber maxBid) ++ "\n" ++
                  "Ask = " ++ (formatNumber minAsk) ++ "\n" ++
                  "Spr = " ++ (formatNumber ((-) <$> minAsk <*> maxBid))
           where formatNumber Nothing = "nothing"
                 formatNumber (Just n) = printf "%.8F" (fromRational n :: Double)

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



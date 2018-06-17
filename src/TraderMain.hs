
module Main where

import           Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as CC

import       Bittrex.Conduit
import           Bittrex.Proto
import qualified Bittrex.ProtoUtil as PU
import           Bittrex.Reader
import           Bittrex.TimeUtil (mkZeroTime)
import           Data.IORef
import           Text.Printf

main = do
  let marketList = [("ETH", "BTC")] --,("BCC", "BTC"),("ETH", "BTC")]
  runConduit $ dataSource
--    .| CC.mapM_ (\(TimedMsg t m)-> putStrLn $ show $ PU.marketName m)
    .| CC.filter (\(TimedMsg t m)-> PU.marketName m == ("XMR","BTC"))
    .| CC.scanlM collectFillsT (Timed mkZeroTime [])
    .| zrateC
    .| CC.mapM_ (\x -> putStrLn $ displayZRate x) 
    .| sinkNull

displayZRate :: Timed (Maybe Double) -> String
displayZRate (Timed t x) = 
  concat ["[", show t, "] ", "ZRate = ", show x]
zrateC :: Monad m => ConduitT (Timed [BtrFillD]) (Timed (Maybe Double)) m ()
zrateC =
  loop
  where
    loop = do
      tradesM <- await
      case tradesM of
        Nothing -> return ()
        Just timedTrades -> do
          let (Timed t trades) = timedTrades
          let prices = map (fromRational . btrFillDOrderRate) trades
          let zr = zrate prices
          yield $ Timed t zr
          loop

 
zrate :: [Double] -> Maybe Double
zrate xs = do
  ma5 <- ma 5 xs
  ma50 <- ma 30 xs
  sigma50 <- mstd 30 xs
  return $ (ma5 - ma50) / sigma50

mstd n xs = do
  mean <- ma n xs
  let squareDiff = (flip map xs $ (\x -> (x-mean)**2))
  avgSquare <- ma n squareDiff
  return $ sqrt avgSquare

ma :: Int -> [Double] -> Maybe Double
ma n xs = let ss = take n xs
              total = sum ss
              in if length ss < n then Nothing else Just $ total / (fromIntegral n)

data FState = FState {
  btcAmount :: IORef Double
  , xmrAmount :: IORef Double
  , lastPrice :: IORef Double
  , buyCount :: IORef Int
} deriving (Eq)

printState s = do
  btc <- readIORef $ btcAmount s
  xmr <- readIORef $ xmrAmount s
  buys <- readIORef $ buyCount s
  
  putStrLn $ "BTC=" ++ (fmt btc) ++ ", XMR=" ++ (fmt xmr) ++ ", buys:" ++ show buys

fmt n = printf "%.8F" n

initState = do
  btc <- newIORef 1
  xmr <- newIORef 0
  lastPrice <- newIORef 0
  buys <- newIORef 0
  return $ FState btc xmr lastPrice buys

buy state p ts = do
  btc <- readIORef $ btcAmount state
  xmr <- readIORef $ xmrAmount state
  buys <- readIORef $ buyCount state
  let p1 = p * 1.005
  if btc > 0 then do
    writeIORef (btcAmount state) 0
    writeIORef (xmrAmount state) (xmr + (btc/p1))
    writeIORef (lastPrice state) p1
    writeIORef (buyCount state) (buys+1)
    putStrLn $ "["++ (show $ ts) ++"] Buying XMR at price: " ++ fmt p1 ++ " amount " ++ (show $ btc/p1)
  else
    return ()

sell state p ts = do
  btc <- readIORef $ btcAmount state
  xmr <- readIORef $ xmrAmount state
  if xmr > 0 then do
    writeIORef (btcAmount state) (btc + (xmr*p))
    writeIORef (xmrAmount state) 0
    writeIORef (lastPrice state) 0
    putStrLn $ "["++ (show $ ts) ++"] Selling XMR at price: " ++ fmt p ++ " amount " ++ (show $ xmr*p) ++ " total: " ++ show (btc + (xmr*p))
  else
    return ()

meanReverseC :: FState -> ConduitT (Timed [BtrFillD]) b IO ()
meanReverseC state = undefined
--  getZipConduit (go <$> ZipConduit (mapC id) <*> ZipConduit zrateC)

go :: (Timed [BtrFillD]) -> (Timed (Maybe Double)) -> IO ()
go (Timed t1 trades) (Timed t2 zscoreM) = do
          liftIO $ putStrLn $ show t1
{--
          case zscoreM of
            Just zscore -> do
                            let c = head $ reverse trades
                            let price = fromRational $ btrFillDOrderRate  c
                            lp <- readIORef $ lastPrice state
                            if (zscore < -1.5)  then
                              buy state price $ t2 c
                            else if (zscore > -0) &&  (price > lp) then
                              sell state price $ time c
                            else
                              return ()
            Nothing -> return ()
--}
{--
meanStrategy2 :: FState -> [OHLCV] -> IO ()
meanStrategy2 state xs = do
  let zscoreM = zrate $ map open xs
  case zscoreM of
    Just zscore -> do
                    let c = head xs
                    let price = open c
                    lp <- readIORef $ lastPrice state
                    if (zscore < -1.5)  then
                      buy state price $ time c
                    else if (zscore > -0) &&  (price > lp) then
                      sell state price $ time c
                    else
                      return ()
    Nothing -> return ()
--}

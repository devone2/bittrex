module Model.MarketModel where

import Data.List (deleteBy)
import Data.Maybe (listToMaybe)
import GHC.Generics (Generic)
import qualified Data.Map as M

data Market = Market {
    marketName :: String
  , asks :: MarketOrders
  , bids :: MarketOrders
  , fills :: MarketFills
} deriving (Eq, Show)

newtype MarketOrders = MarketOrders [BtrOrder] deriving (Eq, Show)
newtype MarketFills = MarketFills [MarketFill] deriving (Eq, Show)

asList (MarketOrders l) = l

data MarketFill = MarketFill {
   marketFillQuantity :: Rational
 , marketFillRate :: Rational
} deriving (Eq, Show)

data BtrOrderType = Bid | Ask deriving (Eq,Show)

data BtrOrder = BtrOrder {
   btrOrdQuantity :: Rational
 , btrOrdRate :: Rational
} deriving (Eq, Generic, Show)

type ExPair = (String, String)

baseCurrency (_, x) = x
foreignCurrency (x, _) = x
  
data ExName = Bittrex | KuCoin
  deriving (Eq, Show, Ord)

type ExMap = M.Map ExPair Market
  
data Exchange = Exchange ExName ExMap

type ExchangeUpdater = Exchange -> IO ()



emptyMarket name = Market name (MarketOrders []) (MarketOrders []) (MarketFills [])

addOrder :: BtrOrder -> MarketOrders -> MarketOrders
addOrder x (MarketOrders ms) = MarketOrders $ (x : ms)

replaceOrder :: BtrOrder -> MarketOrders -> MarketOrders
replaceOrder x m = addOrder x (deleteOrder x m)

deleteOrder :: BtrOrder -> MarketOrders -> MarketOrders
deleteOrder x (MarketOrders m) = MarketOrders $ (deleteBy sameRate x m)

sameRate :: BtrOrder -> BtrOrder -> Bool
sameRate o1 o2 = (btrOrdRate o1) == (btrOrdRate o2)

addMarketOrder :: BtrOrderType -> BtrOrder -> Market -> Market
addMarketOrder t o m
  | t == Ask = m { asks = addOrder o (asks m)}
  | t == Bid = m { bids = addOrder o (bids m)}

getAskBid :: Market -> (Maybe Rational, Maybe Rational)
getAskBid m = (minimum <$> (rates Ask),  
               maximum <$> (rates Bid))
              where ords t
                      | t == Ask = asList $ asks m
                      | t == Bid = asList $ bids m
                    rates t = maybeList $ map btrOrdRate (ords t)
  

maybeList :: [a] -> Maybe [a]
maybeList xs
  | null xs = Nothing
  | otherwise = Just xs

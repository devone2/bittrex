{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Bittrex.Market where
import           Bittrex.Proto
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
import           Data.Time.Clock (UTCTime)
import qualified Data.Time.Format as TF

import qualified System.IO as IO
import qualified Bittrex.TimeUtil as UTC
import           Data.Time.Format (formatTime, defaultTimeLocale)

-- Module allows to reconstruct market state from deltas
data Timed a = Timed UTCTime a

updateMarket :: Market -> TimedMsg -> Timed Market
updateMarket s (TimedMsg t btrm) =
  case btrm of
    BtrMsgState s -> Timed t $ loadMarket s
    BtrMsgDelta d -> Timed t $ updateMarketByDelta s d


loadMarket :: BtrState -> Market
loadMarket s = let m = emptyMarket "BTC-EUR"
                   m1 = foldl (\m' x -> addMarketOrder Ask x m') m (btrStateSells s)
               in foldl (\m' x -> addMarketOrder Bid x m') m1 (btrStateBuys s)

updateMarketByDelta :: Market -> BtrDelta -> Market
updateMarketByDelta m d = let m1 = foldl (\m' x -> updateMarketOrder Ask x m') m (btrDeltaSells d)
                   in foldl (\m' x -> updateMarketOrder Bid x m') m1 (btrDeltaBuys d)


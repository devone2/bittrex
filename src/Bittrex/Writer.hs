{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Bittrex.Writer (
messageWriter, WriterConfig(..), mainWriter
)
where
import           Bittrex.Proto
import           Model.MarketModel
import           Bittrex.Conduit (watchOrderBook)
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

data WriterConfig = WriterConfig {
  wcFileConfig :: String  
} deriving (Eq, Show)

data WriteState = WriteState {
  startTime :: UTCTime
 , file :: IO.Handle
}

messageWriter config c = do 
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

        mkFilename t = concat [wcFileConfig config , formatTime defaultTimeLocale "%F_%T" t, ".dat"]


mainWriter filePrefix = do 
  let marketList = [("ETH", "BTC"),("BCC", "BTC"),("XMR", "BTC")]

  c <- newChan
  forkIO $ forever $ watchOrderBook marketList c

  messageWriter (WriterConfig filePrefix) c

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Bittrex.Reader
  (dataSource,showDataLog2)
where
import           Bittrex.Proto
import qualified Bittrex.ProtoUtil as PU
import           Model.MarketModel
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
import qualified Data.HashMap.Strict as M
import           Data.Hashable (Hashable)
import           Data.List (intersect, union, foldl')
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

import           Data.Binary
import           Data.Binary.Get (ByteOffset)
import           System.Directory (listDirectory)
import           Data.List (sort)


dataSource :: ConduitT i TimedMsg IO ()
dataSource = do
  dataFiles <- liftIO $ sort <$> listDirectory "data/"
--  liftIO $ putStrLn $ unlines $ map (\x -> x ++", ") $ dataFiles

  mapM_ (\file -> do
    msgs <- liftIO $ readMsgFromFile $ ("data/" ++ file)
    yieldMany msgs) dataFiles

displayTimedMsg (TimedMsg t m) =
  "[" ++ (show t) ++ "] msg from market : " ++ (show $ PU.marketName m)
  
showDataLog2 = do
  dataFiles <- sort <$> listDirectory "data/"
  putStrLn $ unlines $ map (\x -> x ++", ") $ dataFiles

  msgs <- concat <$> mapM (readMsgFromFile . ((++) "data/")) dataFiles

  putStrLn $ "Total msgs:" ++ (show $ length msgs)

--  let gs = M.toList $ groupBtrMsgs (\(TimedMsg ts x) -> PU.marketName x) msgs
{--  mapM_ (\g -> do
            putStrLn $ showBtrGroup g
        ) gs
--}   
  


  where showBtrGroup (p, xs) = show p ++ " Records:" ++ (show $ length xs)
{-
readMsgFromFile :: FilePath -> IO [TimedMsg]
readMsgFromFile file = do
  withFile file ReadMode $ \handle -> do
    content <- BL.hGetContents handle
    return $ decodeR2 content
-}


    
readMsgFromFile :: FilePath -> IO [TimedMsg]
readMsgFromFile file = do
  putStrLn $ "Reading file: " ++ file
  decodeR2 <$> BL.readFile file
 
groupBtrMsgs :: (Eq k, Hashable k) => (TimedMsg -> k) -> [TimedMsg] -> M.HashMap k [TimedMsg]
groupBtrMsgs toKey = foldl' updateMap M.empty
  where updateMap m x = M.alter (updateValue x) (toKey x) m
        updateValue x Nothing = Just [x]
        updateValue x (Just ys) = Just $! x:ys


decodeR2 :: BL.ByteString -> [TimedMsg]
decodeR2 c = go c
  where
    go !c
      | BL.null c = []
      | otherwise = let parsed = decodeOrFail c :: Either (BL.ByteString, ByteOffset, String) (BL.ByteString, ByteOffset, TimedMsg)
                    in case parsed of
                        Left (rest, _ , err) -> error $ ("Error when decoding input: " ++ err)
                        Right (rest, _ , !msg) -> msg : go rest

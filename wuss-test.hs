import Wuss

import Control.Concurrent (forkIO)
import Control.Monad (forever, unless, void)
import Data.Text (Text, pack)
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)

import           Network.URL
import           Data.Aeson.Encode.Pretty
import           Data.Aeson
import           Data.Attoparsec
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  putStrLn $ "Relative url: " ++ (exportURL relativeUrl)
  runSecureClient "socket.bittrex.com"  443 (exportURL relativeUrl) ws

relativeUrl = URL HostRelative "/signalr/connect" socketParams


--https://socket.bittrex.com/signalr/connect?transport=webSockets&clientProtocol=1.5&connectionToken=EXn7RPZ9EAk5lYZqzNok65Y%2BnRIflZH8CexgPPHiQGqhmt6JXcd9%2BvqMR5WX38O%2BxzaYO1e%2B3h2%2FyobUlVdzC8hk1GwT92uw%2B6bOkWy39P5MF6Iw&connectionData=%5B%7B%22name%22%3A%22corehub%22%7D%5D&tid=3

socketParams =
  [ ("transport", "webSockets")
  , ("clientProtocol", "1.5")
  , ("connectionToken", "T7oiHK59UuHWCep9SYcwPOmdAsMCJIrDhWYxJ6t4s9/HDuETUGtu8FURaG5/8MTSgVZU5jEqZLWryIy87B+lEVyR+BlHd8z4+l47WdJEry2QtT1S")
  , ("connectionData","[{\"name\":\"corehub\"}]")
  , ("tid","0")
  ]



ws :: ClientApp ()
ws connection = do
    putStrLn "Connected!"

    void . forkIO . forever $ do
        message <- (receiveData connection) :: IO BS.ByteString
        putStrLn "------- Message Received ----------"
        let parsedMsg = parse json message
        case parsedMsg of
          Fail _ _ msg -> error $ "Problem parsing: " ++ msg
          Done _ parsedResult -> do
            BL.putStrLn (encodePretty parsedResult)
        putStrLn "------- Message End ---------------"
    let loop = do
            line <- getLine
            unless (null line) $ do
                sendTextData connection (pack line)
                loop
    loop

    sendClose connection (pack "Bye!")

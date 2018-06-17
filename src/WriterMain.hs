module Main where

import       Bittrex.Writer (mainWriter)
import       System.Environment (getArgs)
main = do
  args <- getArgs
  if (length args /= 1) then
    putStrLn "Required single argument path prefix to store data to"
  else do
    putStrLn "Starting Bittrex writer..."
    mainWriter $ head args

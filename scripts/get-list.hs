import Data.List
import Data.Char (isSpace)

main = do
  content <- readFile "kucoin_btc_list.txt" :: IO String
  putStrLn $ "[" ++ intercalate ", " (map (wrapQuotes . trim) $ lines content) ++ "]"
  where wrapQuotes s = "\"" ++ s ++ "\""
        trim = dropWhileEnd isSpace . dropWhile isSpace

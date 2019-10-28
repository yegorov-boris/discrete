module Main where

import Control.Monad (foldM)

main :: IO ()
main = do
  s <- getLine
  let n = (read s :: Int)
  ss <- foldM readInput [] [1..n]
  putStrLn $ show ss

readInput :: [String] -> Int -> IO [String]
readInput ss _ = do
  s <- getLine
  if
    s == "0"
  then
    return $ s:ss
  else
    getLine >>= \s' -> return $ s':s:ss

--data T =

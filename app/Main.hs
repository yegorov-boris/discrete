module Main where

import Control.Monad (foldM)
import Data.List (foldr, map, sort)
import Data.HashMap (empty, insert, Map)
import Data.String (words)
import Data.Bits (shift)
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)

main :: IO ()
main = do
  s <- getLine
  let n = read s :: Int
  m <- foldM readInput empty [1..n]
  putStrLn $ show m

data T = E | N Int [Int] Integer deriving Show

readInput :: Map Int T -> Int -> IO (Map Int T)
readInput m i = do
  s <- getLine
  if
    s == "0"
  then
    return $ insert i E m
  else
    getLine >>= \s' -> return $ insert i (toNode s s') m

toNode :: String -> String -> T
toNode a b =
  let
    ns = map (\s -> read s :: Int) $ words a
  in
    N (head ns) (sort $ tail ns) (fromBits b)

fromBits :: String -> Integer
fromBits s = foldr processBit 0 s
  where
    processBit '0' n = shift n 1
    processBit '1' n = (shift n 1) + 1
    processBit _   n = n

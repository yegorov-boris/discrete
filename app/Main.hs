module Main where

import Control.Monad (foldM)
import Data.List (foldr, map, sort, maximum)
import Data.HashMap ((!), empty, insert, Map)
import Data.String (words)
import Data.Bits (shiftL, shiftR, setBit, clearBit, testBit)
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)

main :: IO ()
main = do
  s <- getLine
  let n = read s :: Int
  m <- foldM readInput empty [1..n]
  let r = snd $ getResult m n
  putStrLn $ show $ depth m n
  putStrLn $ reverse $ tail $ showIntAtBase 2 intToDigit r ""

data T = E | N Int [Int] Integer deriving Show
type M = Map Int T

readInput :: M -> Int -> IO M
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
    processBit '0' n = shiftL n 1
    processBit '1' n = (shiftL n 1) + 1
    processBit _   n = n

getResult :: M -> Int -> (Int, Integer)
getResult m i = case m ! i of
  (N _ indexes table) -> (inputsCount, mergeTables table (2^inputsCount) tables)
    where
      tables = map (getResult m) indexes
      inputsCount = foldr (\(a, _) b -> a + b) 0 tables
  _ -> (1, 2)

mergeTables :: Integer -> Int -> [(Int, Integer)] -> Integer
mergeTables table range tables = doMergeTables 0 $ shiftL 1 range
  where
    doMergeTables i result | i == range = result
    doMergeTables i result | otherwise  = doMergeTables
      (succ i)
      (if testBit table (code i) then setBit result i else result)
    code i = snd $ foldr getCode (i, 0) tables
    getCode (inputsCount, t) (i, c) = (shiftR i inputsCount, nextC)
      where
        nextC = if c == 0 then currentBit else (shiftL c 1) + currentBit
        currentBit = if testBit t (i `mod` 2^inputsCount) then 1 else 0

depth :: M -> Int -> Int
depth m n = case m ! n of
  E               -> 0
  (N _ indexes _) -> 1 + (maximum $ map (depth m) indexes)

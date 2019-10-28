module Main where

import Control.Monad (foldM)
import Data.List (zipWith, filter, map, sort)
import Data.HashMap (fromList, Map)
import Data.String (words)

main :: IO ()
main = do
  s <- getLine
  let n = read s :: Int
  ss <- foldM readInput [] [1..n]
  let indexed = zipWith (\a b -> (a, toNode b)) [1..] (reverse ss)
  let leaves = sort $ map fst $ filter (isLeave . snd) indexed
  let nodes = fromList indexed :: Map Int T
  putStrLn $ show leaves
  putStrLn $ show nodes

readInput :: [(String, String)] -> Int -> IO [(String, String)]
readInput ss _ = do
  s <- getLine
  if
    s == "0"
  then
    return $ (s, ""):ss
  else
    getLine >>= \s' -> return $ (s, s'):ss

data T = L Bool | N [Int] [Bool] deriving Show

isLeave :: T -> Bool
isLeave (L _) = True
isLeave _     = False

toNode :: (String, String) -> T
toNode ("0", _) = L False
toNode (a, b)   = N
  (map (\s -> read s :: Int) $ tail $ words a)
  (map (== "1") $ words b)

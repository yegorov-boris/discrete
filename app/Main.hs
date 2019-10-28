module Main where

import Control.Monad (foldM)
import Data.List (zip, filter)

main :: IO ()
main = do
  s <- getLine
  let n = (read s :: Int)
  ss <- foldM readInput [] [1..n]
  let leaves = map snd $ filter ((==) "0" . fst . fst) $ zip (reverse ss) [1..]
  putStrLn $ show leaves

readInput :: [(String, String)] -> Int -> IO [(String, String)]
readInput ss _ = do
  s <- getLine
  if
    s == "0"
  then
    return $ (s, ""):ss
  else
    getLine >>= \s' -> return $ (s, s'):ss

--data T =

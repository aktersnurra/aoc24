module Main where

import qualified Data.HashMap.Strict as S
import Data.List (sort, transpose)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Text.Read (readMaybe)

parse :: String -> Maybe [[Int]]
parse = sortLists . map sequenceA . transpose . map (map readMaybe . words) . lines

sortLists :: [Maybe [Int]] -> Maybe [[Int]]
sortLists [Just xs, Just ys] = Just [sort xs, sort ys]
sortLists _ = Nothing

distance :: Int -> Int -> Int
distance x y = abs ((-) x y)

totalDistance :: [[Int]] -> Maybe Int
totalDistance [xs, ys] = Just (sum $ zipWith distance xs ys)
totalDistance _ = Nothing

part1 :: [[Int]] -> Maybe Int
part1 = totalDistance

occurrences :: [Int] -> S.HashMap Int Int
occurrences xs = S.fromListWith (+) [(x, 1) | x <- xs]

calculateScore :: Maybe Int -> Int -> Int
calculateScore (Just x) y = (*) x y
calculateScore Nothing _ = 0

similarityScore :: Int -> Int -> S.HashMap Int Int -> Int
similarityScore acc k hm = (+) acc $ calculateScore (S.lookup k hm) k

part2 :: [[Int]] -> Maybe Int
part2 [xs, ys] = Just (foldl (\acc k -> similarityScore acc k hm) 0 xs)
 where
  hm = occurrences ys
part2 _ = Nothing

solve :: String -> ([[Int]] -> Maybe Int) -> Maybe Int
solve input part = parse input >>= part

getPart :: [String] -> Maybe ([[Int]] -> Maybe Int)
getPart ["1"] = Just part1
getPart ["2"] = Just part2
getPart _ = Nothing

out :: Maybe Int -> IO a
out (Just x) = do
  print x
  exitSuccess
out _ = exitFailure

main :: IO ()
main = do
  args <- getArgs
  contents <- getContents
  let part = getPart args
  out $ part >>= solve contents

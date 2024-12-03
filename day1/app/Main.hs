module Main where

import qualified Data.HashMap.Strict as S
import Data.List (sort, transpose)
import Text.Read (readMaybe)

parse :: String -> Maybe [[Int]]
parse = sortLists . map sequenceA . transpose . map (map readMaybe . words) . lines

sortLists :: [Maybe [Int]] -> Maybe [[Int]]
sortLists [Just xs, Just ys] = Just [sort xs, sort ys]
sortLists _ = Nothing

distance :: Int -> Int -> Int
distance x y = abs ((-) x y)

totalDistance :: Maybe [[Int]] -> Maybe Int
totalDistance (Just [xs, ys]) = Just (sum $ zipWith distance xs ys)
totalDistance _ = Nothing

part1 :: Maybe [[Int]] -> Maybe Int
part1 = totalDistance

occurrences :: [Int] -> S.HashMap Int Int
occurrences xs = S.fromListWith (+) [(x, 1) | x <- xs]

calculateScore :: Maybe Int -> Int -> Int
calculateScore (Just x) y = (*) x y
calculateScore Nothing _ = 0

similarityScore :: Int -> Int -> S.HashMap Int Int -> Int
similarityScore acc k hm = (+) acc $ calculateScore (S.lookup k hm) k

part2 :: Maybe [[Int]] -> Maybe Int
part2 (Just [l, r]) = Just (foldl (\acc k -> similarityScore acc k hm) 0 l)
 where
  hm = occurrences r
part2 _ = Nothing

main :: IO ()
main = do
  contents <- getContents
  let input = parse contents
  print $ part1 input
  print $ part2 input

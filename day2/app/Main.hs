module Main where

import Data.List
import System.Environment (getArgs)

parse :: String -> [[Int]]
parse = filter (not . null) . map (map read . words) . lines

solve :: ([[Int]] -> Int) -> String -> Int
solve part = part . parse

isSafe :: [Int] -> Bool
isSafe xs = all (\x -> x > 0 && x < 4) deltas || all (\x -> x < 0 && x > -4) deltas
 where
  deltas = zipWith (-) xs (tail xs)

part1 :: [[Int]] -> Int
part1 = foldl (\acc x -> if x then acc + 1 else acc) 0 . map (\xs -> isSafe xs)

isSafe2 :: [Int] -> Bool
isSafe2 xs = elem True $ map (\x -> isSafe x) xss
 where
  xss = zipWith (++) (inits xs) (tail (tails xs) ++ [[]])

part2 :: [[Int]] -> Int
part2 = foldl (\acc x -> if x then acc + 1 else acc) 0 . map (\xs -> isSafe2 xs)

getPart :: [String] -> ([[Int]] -> Int)
getPart ["1"] = part1
getPart ["2"] = part2
getPart _ = error "Incorrect part"

main :: IO ()
main = do
  args <- getArgs
  contents <- getContents
  let part = getPart args
  print $ solve part contents

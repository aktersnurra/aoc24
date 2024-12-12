module Main where

import Data.List
import System.Environment (getArgs)
import Text.Regex.TDFA

parse :: String -> [String]
parse = filter (not . null) . lines

solve :: ([String] -> Int) -> String -> Int
solve part = part . parse

muls :: String -> [String]
muls x = getAllTextMatches (x =~ "mul\\([0-9][0-9]?[0-9]?,[0-9][0-9]?[0-9]?\\)")

nums :: String -> [String]
nums x = getAllTextMatches (x =~ "[0-9][0-9]?[0-9]?")

multiply :: [String] -> Int
multiply (x : y : []) = (*) (read x) (read y)

part1 :: [String] -> Int
part1 = sum . map sum . map (map multiply) . map (map nums) . map muls

-- part2 :: [[Int]] -> Int
-- part2 = foldl (\acc x -> if x then acc + 1 else acc) 0 . map (\xs -> isSafe2 xs)

getPart :: [String] -> ([String] -> Int)
getPart ["1"] = part1
-- getPart ["2"] = part2
getPart _ = error "Incorrect part"

main :: IO ()
main = do
  args <- getArgs
  contents <- getContents
  let part = getPart args
  print $ solve part contents

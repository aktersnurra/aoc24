module Main where

import Data.Either (lefts, rights)
import Data.Either.Combinators (mapLeft, mapRight)
import Data.List (sortBy)
import Data.List.Split (splitOn)
import Data.Set (Set, fromList, member)
import System.Environment (getArgs)

readRule :: String -> (Int, Int)
readRule rule = (x, y)
  where
    [x, y] = map read . splitOn "|" $ rule

parse :: String -> (Set (Int, Int), [[Int]])
parse input = (rules, updates)
  where
    [x, y] = splitOn "\n\n" input
    rules = fromList . map readRule . lines $ x
    updates = map (map read . splitOn ",") . lines $ y

solve :: ((Set (Int, Int), [[Int]]) -> Int) -> String -> Int
solve part = part . parse

applyRules :: Set (Int, Int) -> Int -> Int -> Ordering
applyRules rules a b = if member (a, b) rules then LT else GT

sortUpdates :: Set (Int, Int) -> [Int] -> Either [Int] [Int]
sortUpdates rules update
    | valid = Right update
    | otherwise = Left update
  where
    updatePairs = zip update (tail update)
    applyRules' = applyRules rules
    valid = foldl (\acc (a, b) -> if acc then applyRules' a b == LT else acc) True updatePairs

middlePageNumber :: [Int] -> Int
middlePageNumber update = update !! n
  where
    n = div (length update) 2

part1 :: (Set (Int, Int), [[Int]]) -> Int
part1 (rules, updates) = sum . rights . map (mapRight middlePageNumber . sortUpdates rules) $ updates

part2 :: (Set (Int, Int), [[Int]]) -> Int
part2 (rules, updates) = sum . lefts . map (mapLeft (middlePageNumber . sortBy applyRules') . sortUpdates rules) $ updates
  where
    applyRules' = applyRules rules

getPart :: [String] -> ((Set (Int, Int), [[Int]]) -> Int)
getPart ["1"] = part1
getPart ["2"] = part2
getPart _ = error "Incorrect part"

main :: IO ()
main = do
    args <- getArgs
    contents <- getContents
    let part = getPart args
    print $ solve part contents

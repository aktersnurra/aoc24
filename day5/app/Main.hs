module Main where

import Data.Either (lefts, rights)
import Data.Either.Combinators (mapLeft, mapRight)
import Data.List (elemIndex, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (isNothing)
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

followsRule :: (Int, Int) -> [Int] -> Bool
followsRule (r1, r2) update
    | isNothing i1 || isNothing i2 = True
    | Just i2 >= Just i1 = True
    | Just i2 < Just i1 = False
  where
    i1 = elemIndex r1 update
    i2 = elemIndex r2 update
followsRule _ _ = error "Not supported"

validateUpdate :: Set (Int, Int) -> [Int] -> Either [Int] [Int]
validateUpdate rules update =
    if valid
        then
            Right update
        else
            Left update
  where
    valid = foldl (\acc rule -> if acc then followsRule rule update else acc) True rules

middlePageNumber :: [Int] -> Int
middlePageNumber update = update !! n
  where
    n = div (length update) 2

part1 :: (Set (Int, Int), [[Int]]) -> Int
part1 (rules, updates) = sum . rights . map (mapRight middlePageNumber . validateUpdate rules) $ updates

applyRules :: Set (Int, Int) -> Int -> Int -> Ordering
applyRules rules a b = if member (a,b) rules then LT else GT

part2 :: (Set (Int, Int), [[Int]]) -> Int
part2 (rules, updates) = sum . lefts . map (mapLeft (middlePageNumber . sortBy applyRules') . validateUpdate rules) $ updates
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

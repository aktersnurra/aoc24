module Main where

import Data.Array
import Data.List (sort)
import System.Environment (getArgs)

parse :: String -> Array (Int, Int) Char
parse = toArray . filter (not . null) . lines

solve :: (Array (Int, Int) Char -> Int) -> String -> Int
solve part = part . parse

toArray :: [String] -> Array (Int, Int) Char
toArray x = listArray ((0, 0), (n, m)) $ concat x
 where
  n = length x - 1
  m = length (head x) - 1

kernels :: (Int, Int) -> [[(Int, Int)]]
kernels (x, y) = horizontal ++ vertical ++ forwardDiagonal ++ backwardDiagonal
 where
  backwardDiagonal = [[(x', y'), (x' - 1, y' + 1), (x' - 2, y' + 2), (x' - 3, y' + 3)] | x' <- [3 .. x], y' <- [0 .. y - 3]]
  forwardDiagonal = [[(x', y'), (x' + 1, y' + 1), (x' + 2, y' + 2), (x' + 3, y' + 3)] | x' <- [0 .. x - 3], y' <- [0 .. y - 3]]
  vertical = [[(x', y'), (x', y' + 1), (x', y' + 2), (x', y' + 3)] | x' <- [0 .. x], y' <- [0 .. y - 3]]
  horizontal = [[(x', y'), (x' + 1, y'), (x' + 2, y'), (x' + 3, y')] | x' <- [0 .. x - 3], y' <- [0 .. y]]

part1 :: Array (Int, Int) Char -> Int
part1 xs = length . filter (\w -> w == "XMAS" || w == "SAMX") . map (map (xs !)) $ k
 where
  upperBounds = snd $ bounds xs
  k = kernels upperBounds

crossmasKernels :: (Int, Int) -> [[(Int, Int)]]
crossmasKernels (x, y) = [[(x', y'), (x' + 1, y' + 1), (x' + 2, y' + 2), (x' + 2, y'), (x', y' + 2)] | x' <- [0 .. x - 2], y' <- [0 .. y - 2]]

crossmas :: String -> Bool
crossmas [x1, 'A', x3, x4, x5] = all ((== "MS") . sort) [[x1, x3], [x4, x5]]
crossmas _ = False

part2 :: Array (Int, Int) Char -> Int
part2 xs = length . filter crossmas . map (map (xs !)) $ k
 where
  upperBounds = snd $ bounds xs
  k = crossmasKernels upperBounds

getPart :: [String] -> (Array (Int, Int) Char -> Int)
getPart ["1"] = part1
getPart ["2"] = part2
getPart _ = error "Incorrect part"

main :: IO ()
main = do
  args <- getArgs
  contents <- getContents
  let part = getPart args
  print $ solve part contents

module Main where

import System.Environment (getArgs)
import Text.Regex.TDFA

solve :: (String -> Int) -> String -> Int
solve part = part

muls :: String -> [String]
muls x = getAllTextMatches (x =~ "mul\\([0-9][0-9]?[0-9]?,[0-9][0-9]?[0-9]?\\)")

nums :: String -> [String]
nums x = getAllTextMatches (x =~ "[0-9][0-9]?[0-9]?")

multiply :: [String] -> Int
multiply [x, y] = (*) (read x) (read y)
multiply _ = error "err"

part1 :: String -> Int
part1 = sum . (map multiply) . (map nums) . muls

instructions :: String -> [String]
instructions x = getAllTextMatches (x =~ "do|don\'t|mul\\([0-9][0-9]?[0-9]?,[0-9][0-9]?[0-9]?\\)")

multiply2 :: (Bool, Int) -> String -> (Bool, Int)
multiply2 (True, b) ('m' : 'u' : 'l' : y) = (True, total)
 where
  mul = multiply $ nums y
  total = b + mul
multiply2 (False, b) ('m' : 'u' : 'l' : _) = (False, b)
multiply2 (_, b) ['d', 'o'] = (True, b)
multiply2 (_, b) ['d', 'o', 'n', '\'', 't'] = (False, b)
multiply2 (a, b) _ = (a, b)

part2 :: String -> Int
part2 = snd . foldl multiply2 (True, 0) . instructions

getPart :: [String] -> (String -> Int)
getPart ["1"] = part1
getPart ["2"] = part2
getPart _ = error "Incorrect part"

main :: IO ()
main = do
  args <- getArgs
  contents <- getContents
  let part = getPart args
  print $ part contents

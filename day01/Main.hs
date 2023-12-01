module Main where

import qualified Common
import Data.Char (isDigit)
import Data.List (isPrefixOf)

main :: IO ()
main = do
    putStrLn $ "-- Solving day01 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = lines input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = sum
                . map (\digits -> read ((head digits):[last digits]))
                $ map (filter isDigit) parsedInput :: Int
    let answer2 = sum
                . map (\digits -> (head digits) * 10 + (last digits))
                $ map toDigits parsedInput :: Int

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2


solve1 :: String -> Int
solve1 xs = read ((head digits):[last digits])
    where digits = filter isDigit xs


toDigits :: String -> [Int]
toDigits [] = []
toDigits (x:xs)
  | isDigit x = (read [x]):(toDigits xs)
toDigits xs
  | isPrefixOf "one" xs   = 1:(toDigits $ tail xs)
  | isPrefixOf "two" xs   = 2:(toDigits $ tail xs)
  | isPrefixOf "three" xs = 3:(toDigits $ tail xs)
  | isPrefixOf "four" xs  = 4:(toDigits $ tail xs)
  | isPrefixOf "five" xs  = 5:(toDigits $ tail xs)
  | isPrefixOf "six" xs   = 6:(toDigits $ tail xs)
  | isPrefixOf "seven" xs = 7:(toDigits $ tail xs)
  | isPrefixOf "eight" xs = 8:(toDigits $ tail xs)
  | isPrefixOf "nine" xs  = 9:(toDigits $ tail xs)
  | otherwise             = toDigits $ tail xs

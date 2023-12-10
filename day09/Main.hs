module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day09 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let diff xs = map (uncurry (-)) $ zip (tail xs) xs
    let history = takeWhile (not . all (==0)) . iterate diff
    let answer1 = sum $ map (foldr (\x -> (last x +)) 0 . history) parsedInput
    let answer2 = sum $ map (foldr (\x -> (head x -)) 0 . history) parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [[Int]]
parse input = map (map read . words) $ lines input

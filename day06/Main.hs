module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day06 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let getWinningOptions (t,d) = filter (>d) $ getOptions t
            where getOptions x = map (\i -> (x-i) * i) [0..x]
    let solve = product . map length . map getWinningOptions
    let answer1 = solve parsedInput
    let concatNumber :: [Int] -> Int
        concatNumber ns = read . concat $ map show ns
    let answer2 = solve [(concatNumber times, concatNumber distances)]
            where (times,distances) = unzip parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [(Int,Int)]
parse input = let (time,distance) = Common.splitOnceOn "\n" input
                  time' = map read . words . snd $ Common.splitOnceOn ": " time
                  distance' = map read . words . snd $ Common.splitOnceOn ": " distance
               in zip time' distance'

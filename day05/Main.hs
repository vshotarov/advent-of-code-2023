module Main where

import qualified Common
import Data.List.Split (chunksOf)
import Data.Char (isDigit)

main :: IO ()
main = do
    putStrLn $ "-- Solving day05 --"
    input <- Common.readInput

    -- Parse
    let parsedInput@(seeds,maps) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let transform maps_ seed = foldl remap seed maps_
            where remap x [] = x
                  remap x ((d,s,r):ules)
                      | x >= s && x < s+r = d + (x - s)
                      | otherwise         = remap x ules

    let answer1 = minimum $ map (transform maps) seeds

    let reversedMaps = reverse $ map (map (\(d,s,r) -> (s,d,r))) maps
    let isInSeeds2 i = any (\[s,r] -> i >= s && i < s+r) $ chunksOf 2 seeds

    let answer2 = Common.firstWhere (isInSeeds2 . transform reversedMaps) [0..]

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Rule = (Int, Int, Int)
type Map = [Rule]

parse :: String -> ([Int], [Map])
parse input = (seeds, maps)
    where input' = filter (not . null) $ lines input
          seeds = map read . words . snd . Common.splitOnceOn ": " $ head input'
          parseRule rule = case (map read $ words rule) of
                             [d,s,r] -> (d,s,r)
                             _       -> error ("malformatted rule - " ++ rule)
          parseMap parsed [] = (parsed, [])
          parseMap parsed (x:xs)
              | isDigit $ head x = parseMap ((parseRule x):parsed) xs
              | otherwise        = (parsed, xs)
          maps = parseMaps $ drop 2 input'
              where parseMaps [] = []
                    parseMaps xs = let (rules,xs') = parseMap [] xs
                                    in [rules] ++ (parseMaps xs')

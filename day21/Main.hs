module Main where

import qualified Common
import qualified Data.Map as M
import qualified Data.Set as S

import Debug.Trace

main :: IO ()
main = do
    putStrLn $ "-- Solving day21 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = length $ reachable parsedInput 64
    let answer2 = a*(n^2) + b*n + c
            where steps = 26501365
                  size = 1 + (maximum . map fst $ M.keys parsedInput)
                  n = steps `div` size
                  nn = steps `mod` size
                  f0 = length $ reachable parsedInput (nn + 0*size)
                  f1 = length $ reachable parsedInput (nn + 1*size)
                  f2 = length $ reachable parsedInput (nn + 2*size)
                  d0 = f1 - f0
                  d1 = f2 - f1 - d0
                  a = d1 `div` 2
                  b = d0 - a 
                  c = f0

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Point = (Int,Int)
type Grid = M.Map Point Char

reachable :: Grid -> Int -> [Point]
reachable grid maxSteps = go S.empty [(0,start)]
    where parity = maxSteps `mod` 2
          maxX = maximum . map fst $ M.keys grid
          start = (maxX `div` 2, maxX `div` 2)
          get (x,y) = M.findWithDefault '#' (x `mod` (maxX+1), y `mod` (maxX+1)) grid
          go _ [] = []
          go seen ((s,p@(x,y)):explore)
            | s > maxSteps = go seen explore
            | (x,y) `S.member` seen = go seen explore
            | otherwise = let ns = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
                              ns' = filter ((/='#') . get) ns
                              explore' = explore ++ (map (\n -> (s+1,n)) ns')
                           in if s `mod` 2 == parity
                                 then p:(go (S.insert (x,y) seen) explore')
                                 else go (S.insert (x,y) seen) explore'

parse :: String -> Grid
parse input = M.fromList . concat
            . zipWith (\y -> zipWith (\x c -> ((x,y),c)) [0..]) [0..]
            $ lines input

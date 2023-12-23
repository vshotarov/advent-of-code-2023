module Main where

import qualified Common
import qualified Data.Map as M
import qualified Data.Set as S

import Debug.Trace

main :: IO ()
main = do
    putStrLn $ "-- Solving day23 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = walk parsedInput
    let answer2 = "not solved yet"

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> M.Map (Int,Int) Char
parse input = M.fromList . concat $ zipWith (\y -> zipWith (\x c -> ((x,y),c)) [0..]) [0..] $ lines input

walk :: M.Map (Int,Int) Char -> Int
walk grid = go 0 [(S.empty, (1,0))]
    where (maxX,maxY) = (maximum . map fst $ M.keys grid, maximum . map snd $ M.keys grid)
          get = (grid M.!)
          go best [] = best
          go best ((path,p@(x,y)):explore)
            | x < 0 || x > maxX || y < 0 || y > maxY
                    || get p == '#' || p `S.member` path = go best explore
            | p == (maxX-1, maxY) && S.size path > best = trace (show (S.size path)) $ go (max best $ S.size path) explore
            | p == (maxX-1, maxY) = go (max best $ S.size path) explore
            | otherwise = let path' = S.insert p path
                              ns = case get p of
                                     '>' -> [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
                                     'v' -> [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
                                     '.' -> [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
                                     _   -> []
                              --explore' = explore ++ (map (\n -> (path',n)) ns)
                              explore' = (map (\n -> (path',n)) ns) ++ explore
                           in go best explore'

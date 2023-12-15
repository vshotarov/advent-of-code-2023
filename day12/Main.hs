module Main where

import qualified Common
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.List (intercalate)

main :: IO ()
main = do
    putStrLn $ "-- Solving day12 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = sum $ map (snd . uncurry (arrangements M.empty)) parsedInput
    let answer2 = sum $ map (snd . uncurry (arrangements M.empty))
                $ map (\(s,g) -> (intercalate "?" . take 5 $ repeat s, concat . take 5 $ repeat g)) parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Cache = M.Map (String,[Int]) Int

parse :: String -> [(String,[Int])]
parse = map parseOne . lines
    where parseOne line = let (springs,groups) = Common.splitOnceOn " " line
                           in (springs, map read $ splitOn "," groups)

arrangements :: Cache -> String -> [Int] -> (Cache,Int)
arrangements cache string groups
    | M.member (string,groups) cache = (cache,cache M.! (string,groups))
arrangements cache s []           = (M.insert (s,[]) a cache,a)
                                    where a = fromEnum ('#' `notElem` s)
arrangements cache s (g:gs)
    | length s < g                = (M.insert (s,g:gs) 0 cache,0)
arrangements cache ('.':s) gs     = (M.insert ('.':s,gs) a cache',a)
                                    where (cache',a) = arrangements cache s gs
arrangements cache ('?':s) gs     = (M.insert ('?':s,gs) (a1+a2) cache'', a1+a2)
                                    where (cache',a1)  = arrangements cache  ('.':s) gs
                                          (cache'',a2) = arrangements cache' ('#':s) gs
arrangements cache s (g:gs)       = if '.' `elem` g1 || (not (null g2) && head g2 == '#')
                                       then (M.insert (s,g:gs) 0 cache, 0)
                                       else let (cache',a) = arrangements cache ('.':drop 1 g2) gs
                                             in (M.insert (s,g:gs) a cache', a)
                                    where (g1,g2) = splitAt g s
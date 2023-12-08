module Main where

import qualified Common
import qualified Data.Map as M

main :: IO ()
main = do
    putStrLn $ "-- Solving day08 --"
    input <- Common.readInput

    -- Parse
    let parsedInput@(instructions,nodes) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let step (is,n) = (is',n')
            where is' = (tail is ++ [head is])
                  n' = case head is of
                            'L' -> fst (nodes M.! n)
                            _   -> snd (nodes M.! n)
    let solve1 endRule startNode = length . takeWhile (not . endRule)
                                 $ iterate step (instructions,startNode)
    let answer1 = solve1 ((=="ZZZ") . snd) "AAA"
    let startNodes = filter ((=='A') . last) $ M.keys nodes
    let answer2 = foldr1 lcm $ map (solve1 ((=='Z') . last . snd)) startNodes

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> (String, M.Map String (String,String))
parse input =(head instructions, M.fromList $ map parseNode nodes)
    where (instructions,nodes) = Common.splitOnceOn [""] $ lines input
          parseNode node = (n, (Common.splitOnceOn ", " . tail $ init ns))
              where (n,ns) = Common.splitOnceOn " = " node

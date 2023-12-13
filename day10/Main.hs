module Main where

import qualified Common
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do
    putStrLn $ "-- Solving day10 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let loop = findLoop parsedInput
    let answer1 = length loop `div` 2

    let (minX,maxX,minY,maxY) = (minimum xs, maximum xs, minimum ys, maximum ys)
            where (xs,ys) = unzip loop
    let edges = zipWith (\(p1x,p1y) (p2x,p2y) -> (p2x-p1x, p2y-p1y))
                    loop (tail loop ++ [head loop])
    let loopSet = S.fromList loop
    let insideBorderPoints = S.difference
                                (S.fromList (if all isInBBox withClockwiseNormals
                                                then withClockwiseNormals
                                                else withCounterClockwiseNormals))
                                loopSet
            where isInBBox = (\(x,y) -> x >= minX && x <= maxX
                                     && y >= minY && y <= maxY)
                  normals = map (\(x,y) -> (-y,x)) edges
                  flippedNormals = map (\(x,y) -> (-x,-y)) normals
                  go [] = []
                  go [(p,n)] = [addV p n]
                  go ((p1,n1):(p2,n2):xs)
                    | n1 /= n2  = addV p1 n1:addV p2 n1:addV p2 (addV n1 n2):go ((p2,n2):xs)
                    | otherwise = addV p1 n1:go ((p2,n2):xs)
                  withClockwiseNormals = go (zip loop normals ++ [(head loop, head normals)])
                  withCounterClockwiseNormals =
                    go (zip loop flippedNormals ++ [(head loop, head flippedNormals)])

    let answer2 = S.size
                $ S.difference (bfs loopSet $ S.toList insideBorderPoints) loopSet

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Vec = (Int,Int)
type Point = Vec
type Grid = M.Map Point Char
type Loop = [Point]

parse :: String -> Grid
parse = foldLines . zip [0..] . lines
    where foldLine y grid = foldr (\(x,c) -> M.insert (x,y) c) grid . zip [0..]
          foldLines = foldr (\(y,line) acc -> foldLine y acc line) M.empty

findLoop :: Grid -> Loop
findLoop grid = start:(map fst . takeWhile ((/=start) . fst)
                     . iterate step $ step (start,first))
    where start@(sx,sy) = fst . head . filter ((=='S') . snd) $ M.toList grid
          first = fst . head . filter (\(p,rule) -> (grid M.! p) `elem` rule)
                $ zip [(sx,sy-1),(sx+1,sy),(sx,sy+1),(sx-1,sy)]
                      ["|7F","-J7","|LJ","-FL"]
          step ((px,py),p@(x,y)) = (p,case grid M.! p of
                                        '-' -> (x+x-px,y)
                                        '|' -> (x,y+y-py)
                                        '7' -> if x > px then (x,y+1)
                                                         else (x-1,y)
                                        'F' -> if x < px then (x,y+1)
                                                         else (x+1,y)
                                        'J' -> if x > px then (x,y-1)
                                                         else (x-1,y)
                                        'L' -> if x < px then (x,y-1)
                                                         else (x+1,y)
                                        c -> error ("Can't step over " ++ [c]))

addV :: Vec -> Vec -> Vec
addV (x1,y1) (x2,y2) = (x2+x1,y2+y1)

bfs :: S.Set Point -> [Point] -> S.Set Point
bfs seen [] = seen
bfs seen (p@(x,y):explore)
    | S.member p seen = bfs seen explore
    | otherwise       = let ns = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
                         in bfs (S.insert p seen) $ ns ++ explore
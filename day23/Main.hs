module Main where

import qualified Common
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do
    putStrLn $ "-- Solving day23 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let (maxX,maxY) = (maximum . map fst $ M.keys parsedInput,
                       maximum . map snd $ M.keys parsedInput)
    let answer1 = bfs (maxX-1,maxY) $ condense $ buildGraph False parsedInput --bfs (maxX-1,maxY) $ condense $ buildGraph False parsedInput
    let answer2 = bfs (maxX-1,maxY) $ condense $ buildGraph True parsedInput --bfs (maxX-1,maxY) $ condense $ buildGraph True parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Pos = (Int,Int)
type Grid = M.Map Pos Char
type Graph = M.Map Pos [(Pos,Int)]

parse :: String -> Grid
parse input = M.fromList . concat $ zipWith mapRow [0..] $ lines input
    where mapRow y = zipWith (\x c -> ((x,y),c)) [0..]

buildGraph :: Bool -> Grid -> Graph
buildGraph ignoreSlopes grid = M.mapWithKey gridToGraph $ M.filter (/= '#') grid
    where gridToGraph (x,y) c
            | c == '.' || (c `elem` ">v" && ignoreSlopes) =
                map (\n -> (n,1))
              $ filter (\n -> M.findWithDefault '#' n grid /= '#')
                       [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
            | c == '>' = [((x+1,y),1)]
            | c == 'v' = [((x,y+1),1)]
            | otherwise = error "unreachable"

bfs :: Pos -> Graph -> Int
bfs goal graph = go S.empty 0 [(1,0,0)]
    where go _ best [] = best
          go seen best ((x,y,s):explore)
            | s == -1                      = go (S.delete (x,y) seen) best explore
            | (x,y) == goal                = go seen (max best s) explore
            | S.member (x,y) seen          = go seen best explore
            | otherwise                    =
                          let seen' = S.insert (x,y) seen
                              explore' = [(x,y,-1)] ++ explore
                              explore'' = (map (\((nx,ny),d) -> (nx,ny,d+s)) (graph M.! (x,y))) ++ explore'
                           in go seen' best explore''

condense :: Graph -> Graph
condense graph = let tunnels = M.toList $ M.filter ((==2) . length) graph
                     ((x,y),[((x1,y1),s1),((x2,y2),s2)]) = head tunnels
                     insertF new old = new ++ (filter ((/= (x,y)) . fst) old)
                  in if null tunnels
                        then graph
                        else condense
                           . M.insertWith insertF (x1,y1) [((x2,y2),s1+s2)]
                           . M.insertWith insertF (x2,y2) [((x1,y1),s1+s2)]
                           $ M.delete (x,y) graph

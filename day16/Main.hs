module Main where

import qualified Common
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do
    putStrLn $ "-- Solving day16 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let solve p d = S.size . S.map fst $ walk parsedInput S.empty p d
    let answer1 = solve (0,0) (1,0)
    let (maxX,maxY) = (maximum xs, maximum ys)
            where (xs,ys) = unzip $ M.keys parsedInput
    let startingStates = [((0   ,y),( 1,0)) | y <- [0..maxY]]
                      ++ [((maxX,y),(-1,0)) | y <- [0..maxY]]
                      ++ [((x,   0),(0, 1)) | x <- [0..maxX]]
                      ++ [((x,maxY),(0,-1)) | x <- [0..maxX]]
    let answer2 = maximum $ map (uncurry solve) startingStates

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Vec = (Int,Int)
type Tile = Vec
type State = (Tile,Vec)
type Grid = M.Map Tile Char

parse :: String -> Grid
parse input = M.fromList . concat
            . zipWith (\y -> zipWith (parseRow y) [0..]) [0..] $ lines input
    where parseRow y x c = ((x,y),c)

walk :: Grid -> S.Set State -> Tile -> Vec -> S.Set State
walk grid seen (x,y) (dx,dy)
  | (not $ M.member (x,y) grid) || S.member ((x,y),(dx,dy)) seen = seen
  | grid M.! (x,y) == '.' = walk grid (S.insert ((x,y),(dx,dy)) seen) (x+dx,y+dy) (dx,dy)
  | grid M.! (x,y) == '/' = case (dx,dy) of
                              (1,0)     -> walk grid (S.insert ((x,y),(dx,dy)) seen) (x,y-1) (0,-1)
                              (-1,0)    -> walk grid (S.insert ((x,y),(dx,dy)) seen) (x,y+1) (0,1)
                              (0,1)     -> walk grid (S.insert ((x,y),(dx,dy)) seen) (x-1,y) (-1,0)
                              _         -> walk grid (S.insert ((x,y),(dx,dy)) seen) (x+1,y) (1,0)
  | grid M.! (x,y) == '\\' = case (dx,dy) of
                               (1,0)     -> walk grid (S.insert ((x,y),(dx,dy)) seen) (x,y+1) (0,1)
                               (-1,0)    -> walk grid (S.insert ((x,y),(dx,dy)) seen) (x,y-1) (0,-1)
                               (0,1)     -> walk grid (S.insert ((x,y),(dx,dy)) seen) (x+1,y) (1,0)
                               _         -> walk grid (S.insert ((x,y),(dx,dy)) seen) (x-1,y) (-1,0)
  | grid M.! (x,y) == '|' = case (dx,dy) of
                              (0,_)     -> walk grid (S.insert ((x,y),(dx,dy)) seen) (x+dx,y+dy) (dx,dy)
                              (_,0)     -> let a = walk grid (S.insert ((x,y),(dx,dy)) seen) (x,y+1) (0,1)
                                            in walk grid a (x,y-1) (0,-1)
                              _         -> error "unsupported direction"
  | grid M.! (x,y) == '-' = case (dx,dy) of
                              (_,0)     -> walk grid (S.insert ((x,y),(dx,dy)) seen) (x+dx,y+dy) (dx,dy)
                              (0,_)     -> let a = walk grid (S.insert ((x,y),(dx,dy)) seen) (x+1,y) (1,0)
                                            in walk grid a (x-1,y) (-1,0)
                              _         -> error "unsupported direction"
  | otherwise = error "unsupported tile"

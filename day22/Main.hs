module Main where

import qualified Common
import Data.List.Split (splitOn)
import Data.List (sortOn)
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do
    putStrLn $ "-- Solving day22 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let fallen = fall parsedInput
    let dp = calculateSupports fallen
    let answer1 = foldr (\b -> if not $ elem (S.singleton b) $ M.elems dp
                                  then (+1)
                                  else id) 0 fallen :: Int
    let answer2 = foldr f 0 fallen
            where ff acc b'
                    | M.member b' dp && (S.null $ S.difference (dp M.! b') acc) =
                        S.insert b' acc
                    | otherwise = acc
                  f b acc = acc + (S.size $ foldl ff (S.singleton b) fallen) - 1


    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Vec2 = (Int,Int)
type Vec3 = (Int,Int,Int)
type Brick = (Vec3,Vec3,S.Set Vec2)

parse :: String -> [Brick]
parse input = map parseOne $ lines input
    where parseOne line =
            let (start,end) = Common.splitOnceOn "~" line
                start' = map read $ splitOn "," start
                end' = map read $ splitOn "," end
                zipped = zip start' end'
                min' = map (uncurry min) zipped
                max' = map (uncurry max) zipped
                as2dSet = S.fromList [(x,y) | x <- [(min' !! 0)..(max' !! 0)],
                                              y <- [(min' !! 1)..(max' !! 1)]]
             in case (min',max') of
                  ([minX,minY,minZ],[maxX,maxY,maxZ]) ->
                      ((minX,minY,minZ),(maxX,maxY,maxZ),as2dSet)
                  _ -> error "malformatted input"

collision :: S.Set Vec2 -> S.Set Vec2 -> Bool
collision a b = not . S.null $ S.intersection a b

fall :: [Brick] -> [Brick]
fall = go [] . sortOn (\((_,_,z),_,_) -> z)
    where go _ [] = []
          go fallen (((minX,minY,minZ),(maxX,maxY,maxZ),set):bricks) =
              let newZ = foldr (\(_,(_,_,z),set') -> if collision set set'
                                                        then max (z+1)
                                                        else max 1)
                               1 fallen
                  delta = minZ - newZ
                  b' = ((minX,minY,minZ-delta),(maxX,maxY,maxZ-delta),set)
               in b':(go (b':fallen) bricks)

calculateSupports :: [Brick] -> M.Map Brick (S.Set Brick)
calculateSupports = go M.empty
    where go dp [] = dp
          go dp (b@(_,(_,_,maxZ),set):bricks) =
            let dp' = foldl (\acc b'@((_,_,minZ),_,set') ->
                                 if minZ == maxZ + 1
                                 && collision set set'
                                    then M.insertWith S.union b' (S.singleton b) acc
                                    else acc)
                            dp bricks
             in go dp' bricks

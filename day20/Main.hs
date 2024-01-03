module Main where

import qualified Common
import Data.List.Split (splitOn)
import qualified Data.Map as M

main :: IO ()
main = do
    putStrLn $ "-- Solving day20 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let network = initialiseConjunctions parsedInput
    let answer1 = (\(l,h,_) -> l*h) . last . take 1001 $ iterate iterSolve1 (0,0,network)
    let targetSources = case state $ network M.! "cl" of
                          Left _  -> error "'cl' should be a Conjunction module"
                          Right s -> M.map (\_ -> -1) s
    let answer2 = (\(_,iters,_) -> foldr1 lcm $ M.elems iters)
                . last . takeUntil (\(_,s,_) -> all (/=(-1)) $ M.elems s)
                $ iterate iterSolve2 (0,targetSources,network)
            where takeUntil _ [] = []
                  takeUntil f (x:xs)
                    | f x = [x]
                    | otherwise = x:(takeUntil f xs)

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Name = String
type Connections = [Name]
type Signal = Bool
data Module = Broadcaster Connections
            | FlipFlop Name Signal Connections
            | Conjunction Name (M.Map Name Signal) Connections
            | Output Name
            deriving (Show)
type Network = M.Map Name Module
type AddrFrom = Name
type AddrTo = Name
type InPacket = (Signal, AddrFrom)
type OutPacket = (AddrTo, InPacket)

name :: Module -> Name
name (Broadcaster _)      = "broadcaster"
name (FlipFlop n _ _)     = n
name (Conjunction n _ _)  = n
name (Output n)           = n

state :: Module -> Either Signal (M.Map Name Signal)
state (Broadcaster _)     = error "broadcaster has no state"
state (FlipFlop _ s _)    = Left s
state (Conjunction _ s _) = Right s
state (Output _)          = error "output has no state"

connections :: Module -> Connections
connections (Broadcaster c)     = c
connections (FlipFlop _ _ c)    = c
connections (Conjunction _ _ c) = c
connections (Output _)          = []

process :: Module -> InPacket -> (Module, Maybe Signal)
process m@(Broadcaster _) (signal, _)       = (m, Just signal)
process (FlipFlop n s c) (False, _)         = (FlipFlop n (not s) c, Just (not s))
process (Conjunction n s c) (signal, from)  =
    let s' = M.insert from signal s
        signal' = any not $ M.elems s'
     in (Conjunction n s' c, Just signal')
process m _                                 = (m, Nothing)

receive :: Module -> InPacket -> (Module, [OutPacket])
receive m p = let (m',signal) = process m p
               in case signal of
                    Just s  -> (m', map (\a -> (a, (s, name m))) $ connections m)
                    Nothing -> (m', [])

parse :: String -> Network
parse = foldr parseOne M.empty . lines
    where parseOne line network =
            let (left,right) = Common.splitOnceOn " -> " line
                conns = splitOn ", " right
                module_ = case left of
                            "broadcaster" -> Broadcaster conns
                            ('%':n)       -> FlipFlop n False conns
                            ('&':n)       -> Conjunction n M.empty conns
                            x             -> error ("malformatted input " ++ x)
             in M.insert (name module_) module_
              $ foldr (\destination -> M.insertWith (\_ old -> old) destination (Output destination))
                      network conns

initialiseConjunctions :: Network -> Network
initialiseConjunctions network = foldr initConj network $ M.elems network
    where initConj (Conjunction n _ c) network' =
            let sources = M.keys $ M.filter (any (==n) . connections) network
             in M.insert n (Conjunction n (M.fromList . zip sources $ repeat False) c) network'
          initConj _ network' = network'

iterSolve1 :: (Int,Int,Network) -> (Int,Int,Network)
iterSolve1 = go [("broadcaster", (False, "button"))]
    where go [] out = out
          go ((to,packet@(signal,_)):packets) (lows,highs,network) =
              let (m',newPackets) = receive (network M.! to) packet
                  lows' = lows + fromEnum (not signal)
                  highs' = highs + fromEnum signal
               in go (packets ++ newPackets) (lows',highs',(M.insert to m' network))

iterSolve2 :: (Int, M.Map Name Int, Network) -> (Int, M.Map Name Int, Network)
iterSolve2 (iterIn, sourcesIn, networkIn) =
    go [("broadcaster", (False, "button"))] (iterIn+1, sourcesIn, networkIn)
    where go [] out = out
          go ((to,packet@(signal,from)):packets) (iter,sources,network) =
              let m = network M.! to
                  (m',newPackets) = receive m packet
                  sources' = if to == "cl" && signal && M.findWithDefault (-2) from sources == -1
                                then M.insert from iter sources
                                else sources
               in go (packets ++ newPackets) (iter,sources',(M.insert to m' network))

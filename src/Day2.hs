module Day2(part1,replace) where
import Debug.Trace

part1 :: [Int] -> Int
part1 input = (computeState input) !! 0

computeState :: [Int] -> [Int]
computeState state = compute 0 state

compute :: Int -> [Int] -> [Int]
compute i state
 | opCode == 1 = compute (i+4) $ replace state posRes $ (state!!pos1) + (state!!pos2)
 | opCode == 2 = compute (i+4) $ replace state posRes $ (state!!pos1) * (state!!pos2)
 | opCode == 99 = state
 | otherwise = error $ trace (show state) $ "Unknown opCode: " ++ (show opCode)
 where opCode = state!!i
       pos1 = state!!(i+1)
       pos2 = state!!(i+2)
       posRes = state!!(i+3)

replace :: [Int] -> Int -> Int -> [Int]
replace list i value = concat [start,(value:end)]
  where start = take i list
        end = drop (i+1) list
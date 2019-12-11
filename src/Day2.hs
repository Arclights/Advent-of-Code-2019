module Day2(part1,
            part2) where
import Data.Maybe
import IntComputer

part1 :: [Int] -> Int
part1 input = calculate $ prep input 12 2

part2 :: Int -> [Int] -> Int
part2 goalValue state
  | isJust result = (100 * (fst unpackedResult)) + (snd unpackedResult)
  | otherwise = error "Could not find solution"
  where result = testNouns goalValue state [0..((length state)-1)] [0..((length state)-1)]
        unpackedResult = fromJust result
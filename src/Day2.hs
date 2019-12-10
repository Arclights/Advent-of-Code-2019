module Day2(part1,
            part2,
            calculate) where
import Debug.Trace
import Data.Maybe

part1 :: [Int] -> Int
part1 input = calculate $ prep input 12 2

prep :: [Int] -> Int -> Int -> [Int]
prep state noun verb = replace (replace state 2 verb) 1 noun

calculate :: [Int] -> Int
calculate input = (computeState input) !! 0

computeState :: [Int] -> [Int]
computeState state = compute 0 state

compute :: Int -> [Int] -> [Int]
compute i state
 | opCode == 1 = compute (i+4) $ replace state posRes $ (state!!pos1) + (state!!pos2)
 | opCode == 2 = compute (i+4) $ replace state posRes $ (state!!pos1) * (state!!pos2)
 | opCode == 99 = state
 | otherwise = error $ "Unknown opCode: " ++ (show opCode)
 where opCode = state!!i
       pos1 = state!!(i+1)
       pos2 = state!!(i+2)
       posRes = state!!(i+3)

replace :: [Int] -> Int -> Int -> [Int]
replace list i value = concat [start,(value:end)]
  where start = take i list
        end = drop (i+1) list
        
part2 :: Int -> [Int] -> Int
part2 goalValue state
  | isJust result = (100 * (fst unpackedResult)) + (snd unpackedResult)
  | otherwise = error "Could not find solution"
  where result = testNouns goalValue state [0..((length state)-1)] [0..((length state)-1)]
        unpackedResult = fromJust result

testNouns :: Int -> [Int] -> [Int] -> [Int] -> Maybe (Int, Int)
testNouns _ _ [] _ = Nothing
testNouns goalValue state (noun:nouns) verbs
  | isJust result = result
  | otherwise = testNouns goalValue state nouns verbs
  where result = testVerbs goalValue state noun verbs

testVerbs :: Int -> [Int] -> Int -> [Int] -> Maybe (Int, Int)
testVerbs _ _ noun [] = Nothing
testVerbs goalValue state noun (verb:verbs)
  | result == goalValue = Just (noun, verb)
  | otherwise = testVerbs goalValue state noun verbs
  where result = calculate $ prep state noun verb 
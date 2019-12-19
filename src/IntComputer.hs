module IntComputer(calculate, prep, testNouns) where

import Data.Maybe
import Debug.Trace

prep :: [Int] -> Int -> Int -> [Int]
prep state noun verb = replace (replace state 2 verb) 1 noun

calculate :: [Int] -> [Int] -> (Int, [Int])
calculate input program = (state !! 0, output)
  where (state, output) = computeState input program

computeState :: [Int] -> [Int] -> ([Int], [Int])
computeState input state = compute 0 input [] state

compute :: Int -> [Int] -> [Int] -> [Int] -> ([Int], [Int])
compute pointer input output state
 | opCode == 1 = add pointer input output state param1 param1Mode param2 param2Mode param3
 | opCode == 2 = multiply pointer input output state param1 param1Mode param2 param2Mode param3
 | opCode == 3 = store pointer input output state param1
 | opCode == 4 = sendToOutput pointer input output state param1 param1Mode
 | opCode == 5 = jumpIfTrue pointer input output state param1 param1Mode param2 param2Mode
 | opCode == 6 = jumpIfFalse pointer input output state param1 param1Mode param2 param2Mode
 | opCode == 7 = lessThan pointer input output state param1 param1Mode param2 param2Mode param3
 | opCode == 8 = equals pointer input output state param1 param1Mode param2 param2Mode param3
 | opCode == 99 = (state, output) 
 | otherwise = error $ "Unknown opCode: " ++ (show opCode)
 where instruction = state!!pointer
       param1 = state!!(pointer + 1)
       param2 = state!!(pointer + 2)
       param3 = state!!(pointer + 3)
       (opCode, param1Mode, param2Mode, param3Mode) = parseInstruction $ state!!pointer

add :: Int -> [Int] -> [Int] -> [Int] -> Int -> Int -> Int -> Int -> Int -> ([Int], [Int])
add pointer input output state param1 param1Mode param2 param2Mode param3 = compute updatedPointer input output updatedState
  where input1 = parseParameter state param1 param1Mode
        input2 = parseParameter state param2 param2Mode
        result = input1 + input2
        updatedState = replace state param3 result
        updatedPointer = pointer + 4
        
multiply :: Int -> [Int] -> [Int] -> [Int] -> Int -> Int -> Int -> Int -> Int -> ([Int], [Int])
multiply pointer input output state param1 param1Mode param2 param2Mode param3 = compute updatedPointer input output updatedState
  where input1 = parseParameter state param1 param1Mode
        input2 = parseParameter state param2 param2Mode
        result = input1 * input2
        updatedState = replace state param3 result
        updatedPointer = pointer + 4
        
store :: Int -> [Int] -> [Int] -> [Int] -> Int -> ([Int], [Int])
store pointer (input:is) output state param1 = compute updatedPointer is output updatedState
 where updatedState = replace state param1 input
       updatedPointer = pointer + 2
       
sendToOutput :: Int -> [Int] -> [Int] -> [Int] -> Int -> Int -> ([Int], [Int])
sendToOutput pointer input output state param1 param1Mode = compute updatedPointer input updatedOutput state
  where input1 = parseParameter state param1 param1Mode
        updatedOutput = (input1:output)
        updatedPointer = pointer + 2
 
jumpIfTrue :: Int -> [Int] -> [Int] -> [Int] -> Int -> Int -> Int -> Int -> ([Int], [Int])
jumpIfTrue pointer input output state param1 param1Mode param2 param2Mode = compute updatedPointer input output state
 where input1 = parseParameter state param1 param1Mode
       input2 = parseParameter state param2 param2Mode
       updatedPointer
         | input1 /= 0 = input2
         | otherwise = pointer + 3
        
jumpIfFalse :: Int -> [Int] -> [Int] -> [Int] -> Int -> Int -> Int -> Int -> ([Int], [Int])
jumpIfFalse pointer input output state param1 param1Mode param2 param2Mode = compute updatedPointer input output state
  where input1 = parseParameter state param1 param1Mode
        input2 = parseParameter state param2 param2Mode
        updatedPointer
          | input1 == 0 = input2
          | otherwise = pointer + 3
          
lessThan :: Int -> [Int] -> [Int] -> [Int] -> Int -> Int -> Int -> Int -> Int -> ([Int], [Int])
lessThan pointer input output state param1 param1Mode param2 param2Mode param3 = compute updatedPointer input output updatedState
  where input1 = parseParameter state param1 param1Mode
        input2 = parseParameter state param2 param2Mode
        updatedState
          | input1 < input2 = replace state param3 1
          | otherwise = replace state param3 0
        updatedPointer = pointer + 4
        
equals :: Int -> [Int] -> [Int] -> [Int] -> Int -> Int -> Int -> Int -> Int -> ([Int], [Int])
equals pointer input output state param1 param1Mode param2 param2Mode param3 = compute updatedPointer input output updatedState
  where input1 = parseParameter state param1 param1Mode
        input2 = parseParameter state param2 param2Mode
        updatedState
          | input1 == input2 = replace state param3 1
          | otherwise = replace state param3 0
        updatedPointer = pointer + 4
    
parseParameter :: [Int] -> Int -> Int -> Int
parseParameter state param 0 = state !! param
parseParameter state param 1 = param
parseParameter _ _ paramMode = error $ "Unknown parameter mode " ++ (show paramMode)
       
parseInstruction :: Int -> (Int, Int, Int, Int)
parseInstruction instruction = (mod instruction 100, mod (div instruction 100) 10, mod (div instruction 1000) 10, mod (div instruction 10000) 10)

replace :: [Int] -> Int -> Int -> [Int]
replace list i value = concat [start,(value:end)]
  where start = take i list
        end = drop (i+1) list

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
  where (result, _) = calculate [] $ prep state noun verb
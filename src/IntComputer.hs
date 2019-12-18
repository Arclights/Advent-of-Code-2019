module IntComputer(calculate, prep, testNouns) where

import Data.Maybe

prep :: [Int] -> Int -> Int -> [Int]
prep state noun verb = replace (replace state 2 verb) 1 noun

calculate :: [Int] -> [Int] -> (Int, [Int])
calculate input program = (state !! 0, output)
  where (state, output) = computeState input program

computeState :: [Int] -> [Int] -> ([Int], [Int])
computeState input state = compute 0 input state

compute :: Int -> [Int] -> [Int] -> ([Int], [Int])
compute pointer input state
 | opCode == 1 = add pointer input state param1 param1Mode param2 param2Mode param3
 | opCode == 2 = add pointer input state param1 param1Mode param2 param2Mode param3
 | opCode == 99 = (state, []) 
 | otherwise = error $ "Unknown opCode: " ++ (show opCode)
 where instruction = state!!pointer
       param1 = state!!(pointer + 1)
       param2 = state!!(pointer + 2)
       param3 = state!!(pointer + 3)
       (opCode, param1Mode, param2Mode, param3Mode) = parseInstruction $ state!!pointer
       
add :: Int -> [Int] -> [Int] -> Int -> Int -> Int -> Int -> Int -> ([Int], [Int])
add pointer input state param1 param1Mode param2 param2Mode param3 = compute updatedPointer input updatedState
  where input1 = parseParameter state param1 param1Mode
        input2 = parseParameter state param2 param2Mode
        result = input1 + input2
        updatedState = replace state param3 result
        updatedPointer = pointer + 4
        
multiply :: Int -> [Int] -> [Int] -> Int -> Int -> Int -> Int -> Int -> ([Int], [Int])
multiply pointer input state param1 param1Mode param2 param2Mode param3 = compute updatedPointer input updatedState
  where input1 = parseParameter state param1 param1Mode
        input2 = parseParameter state param2 param2Mode
        result = input1 * input2
        updatedState = replace state param3 result
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
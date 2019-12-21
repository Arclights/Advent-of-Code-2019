module IntComputer(calculate, prep, testNouns) where

import Data.Maybe

prep :: [Int] -> Int -> Int -> [Int]
prep state noun verb = replace (replace state 2 verb) 1 noun

calculate :: [Int] -> [Int] -> (Int, [Int])
calculate input program = (state !! 0, output)
  where (state, output) = computeState input program

computeState :: [Int] -> [Int] -> ([Int], [Int])
computeState input state = compute 0 0 input [] (state ++ (repeat 0))

compute :: Int -> Int -> [Int] -> [Int] -> [Int] -> ([Int], [Int])
compute pointer relativeBase input output state
 | opCode == 1 = add pointer relativeBase input output state param1 param1Mode param2 param2Mode param3 param3Mode
 | opCode == 2 = multiply pointer relativeBase input output state param1 param1Mode param2 param2Mode param3 param3Mode
 | opCode == 3 = store pointer relativeBase input output state param1 param1Mode
 | opCode == 4 = sendToOutput pointer relativeBase input output state param1 param1Mode
 | opCode == 5 = jumpIfTrue pointer relativeBase input output state param1 param1Mode param2 param2Mode
 | opCode == 6 = jumpIfFalse pointer relativeBase input output state param1 param1Mode param2 param2Mode
 | opCode == 7 = lessThan pointer relativeBase input output state param1 param1Mode param2 param2Mode param3 param3Mode
 | opCode == 8 = equals pointer relativeBase input output state param1 param1Mode param2 param2Mode param3 param3Mode
 | opCode == 9 = adjustRelativeBase pointer relativeBase input output state param1 param1Mode
 | opCode == 99 = (state, output) 
 | otherwise = error $ "Unknown opCode: " ++ (show opCode)
 where instruction = state!!pointer
       param1 = state!!(pointer + 1)
       param2 = state!!(pointer + 2)
       param3 = state!!(pointer + 3)
       (opCode, param1Mode, param2Mode, param3Mode) = parseInstruction $ state!!pointer

add :: Int -> Int -> [Int] -> [Int] -> [Int] -> Int -> Int -> Int -> Int -> Int -> Int -> ([Int], [Int])
add pointer relativeBase input output state param1 param1Mode param2 param2Mode param3 param3Mode = compute updatedPointer relativeBase input output updatedState
  where input1 = parseParameter state relativeBase param1 param1Mode
        input2 = parseParameter state relativeBase param2 param2Mode
        input3 = parseWriteParameter state relativeBase param3 param3Mode
        result = input1 + input2
        updatedState = replace state input3 result
        updatedPointer = pointer + 4
        
multiply :: Int -> Int -> [Int] -> [Int] -> [Int] -> Int -> Int -> Int -> Int -> Int -> Int -> ([Int], [Int])
multiply pointer relativeBase input output state param1 param1Mode param2 param2Mode param3 param3Mode = compute updatedPointer relativeBase input output updatedState
  where input1 = parseParameter state relativeBase param1 param1Mode
        input2 = parseParameter state relativeBase param2 param2Mode
        input3 = parseWriteParameter state relativeBase param3 param3Mode
        result = input1 * input2
        updatedState = replace state input3 result
        updatedPointer = pointer + 4

store :: Int -> Int -> [Int] -> [Int] -> [Int] -> Int -> Int -> ([Int], [Int])
store pointer relativeBase (input:is) output state param1 param1Mode = compute updatedPointer relativeBase is output updatedState
 where input1 = parseWriteParameter state relativeBase param1 param1Mode
       updatedState = replace state input1 input
       updatedPointer = pointer + 2

sendToOutput :: Int -> Int -> [Int] -> [Int] -> [Int] -> Int -> Int -> ([Int], [Int])
sendToOutput pointer relativeBase input output state param1 param1Mode = compute updatedPointer relativeBase input updatedOutput state
  where input1 = parseParameter state relativeBase param1 param1Mode
        updatedOutput = (input1:output)
        updatedPointer = pointer + 2

jumpIfTrue :: Int -> Int -> [Int] -> [Int] -> [Int] -> Int -> Int -> Int -> Int -> ([Int], [Int])
jumpIfTrue pointer relativeBase input output state param1 param1Mode param2 param2Mode = compute updatedPointer relativeBase input output state
 where input1 = parseParameter state relativeBase param1 param1Mode
       input2 = parseParameter state relativeBase param2 param2Mode
       updatedPointer
         | input1 /= 0 = input2
         | otherwise = pointer + 3

jumpIfFalse :: Int -> Int -> [Int] -> [Int] -> [Int] -> Int -> Int -> Int -> Int -> ([Int], [Int])
jumpIfFalse pointer relativeBase input output state param1 param1Mode param2 param2Mode = compute updatedPointer relativeBase input output state
  where input1 = parseParameter state relativeBase param1 param1Mode
        input2 = parseParameter state relativeBase param2 param2Mode
        updatedPointer
          | input1 == 0 = input2
          | otherwise = pointer + 3

lessThan :: Int -> Int -> [Int] -> [Int] -> [Int] -> Int -> Int -> Int -> Int -> Int -> Int -> ([Int], [Int])
lessThan pointer relativeBase input output state param1 param1Mode param2 param2Mode param3 param3Mode = compute updatedPointer relativeBase input output updatedState
  where input1 = parseParameter state relativeBase param1 param1Mode
        input2 = parseParameter state relativeBase param2 param2Mode
        input3 = parseWriteParameter state relativeBase param3 param3Mode
        updatedState
          | input1 < input2 = replace state input3 1
          | otherwise = replace state input3 0
        updatedPointer = pointer + 4

equals :: Int -> Int -> [Int] -> [Int] -> [Int] -> Int -> Int -> Int -> Int -> Int -> Int -> ([Int], [Int])
equals pointer relativeBase input output state param1 param1Mode param2 param2Mode param3 param3Mode = compute updatedPointer relativeBase input output updatedState
  where input1 = parseParameter state relativeBase param1 param1Mode
        input2 = parseParameter state relativeBase param2 param2Mode
        input3 = parseWriteParameter state relativeBase param3 param3Mode
        updatedState
          | input1 == input2 = replace state input3 1
          | otherwise = replace state input3 0
        updatedPointer = pointer + 4

adjustRelativeBase :: Int -> Int -> [Int] -> [Int] -> [Int] -> Int -> Int -> ([Int], [Int])
adjustRelativeBase pointer relativeBase input output state param1 param1Mode = compute updatedPointer updatedRelativeBase input output state
  where input1 = parseParameter state relativeBase param1 param1Mode
        updatedRelativeBase = relativeBase + input1
        updatedPointer = pointer + 2

parseParameter :: [Int] -> Int -> Int -> Int -> Int
parseParameter state relativeBase param 0 = state !! param
parseParameter state relativeBase param 1 = param
parseParameter state relativeBase param 2 = state !! (relativeBase + param)
parseParameter _ _ _ paramMode = error $ "Unknown parameter mode " ++ (show paramMode)

parseWriteParameter :: [Int] -> Int -> Int -> Int -> Int
parseWriteParameter state relativeBase param 0 = param
parseWriteParameter state relativeBase param 2 = relativeBase + param
parseWriteParameter _ _ _ paramMode = error $ "Unknown parameter mode " ++ (show paramMode)
       
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
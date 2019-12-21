module Day17(getIntersections,
             getIntersectionTotal,
             parseImage,
             part1) where

import IntComputer
import Debug.Trace

part1 :: [Int] -> Int
part1 program = getIntersectionTotal $ printImage $ parseImage $ reverse $ trimEmptyLines $ snd $ calculate [] program

parseImage :: [Int] -> [[Char]]
parseImage = toImageMatrix . (map translateToChar)

translateToChar :: Int -> Char
translateToChar 10 = '\n'
translateToChar 35 = '#'
translateToChar 46 = '.'
translateToChar 60 = '<'
translateToChar 62 = '>'
translateToChar 94 = '^'
translateToChar 118 = 'v'
translateToChar num = error ("Unknown char: "++(show num))

toImageMatrix :: [Char] -> [[Char]]
toImageMatrix [] = []
toImageMatrix list = (line:(toImageMatrix rest))
  where line = takeWhile (\x -> x /= '\n') list
        lineLength = length line
        rest = drop 1 $ drop lineLength list

trimEmptyLines :: [Int] -> [Int]
trimEmptyLines (10:xs) = trimEmptyLines xs
trimEmptyLines input = input

printImage :: [[Char]] -> [[Char]]
printImage image = trace ("\n"++(unlines image)) image

getIntersectionTotal :: [[Char]] -> Int
getIntersectionTotal image = sum $ map (\(x,y)-> x * y) $ getIntersections image

getIntersections :: [[Char]] -> [(Int, Int)]
getIntersections image = intersectionSearch image shrunkWidth shrunkWidth shrunkHeight
  where width = length $ image !! 0
        height = length image
        shrunkWidth = width - 2
        shrunkHeight = height - 2

-- Assumes intersections cannot occur on the outer border
intersectionSearch :: [[Char]] -> Int -> Int -> Int -> [(Int, Int)]
intersectionSearch _ _ _ 0 = []
intersectionSearch image width 0 y = intersectionSearch image width width (y - 1)
intersectionSearch image width x y
  | isIntersection = ((x, y):(intersectionSearch image width (x - 1) y))
  | otherwise = intersectionSearch image width (x - 1) y
  where atCurrPos = (image !! y) !! x
        over = (image !! (y - 1)) !! x
        below = (image !! (y + 1)) !! x
        left = (image !! y) !! (x - 1)
        right = (image !! y) !! (x + 1)
        isIntersection = (atCurrPos == '#') && (over == '#') && (below == '#') && (left == '#') && (right == '#')

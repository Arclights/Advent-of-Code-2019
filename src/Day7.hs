module Day7(part1) where

import Data.List.Split
import Data.List.Unique
import Debug.Trace

part1 :: String -> Int
part1 input = calculateTotalOrbits orbits planets
  where orbits = parseOrbits $ lines input
        planets = getPlanets orbits

parseOrbits :: [String] -> [(String,String)]
parseOrbits [] = []
parseOrbits (x:xs) = (pair:(parseOrbits xs))
  where parts = splitOn ")" x
        pair = (head parts, last parts)

getPlanets :: [(String,String)] -> [String]
getPlanets orbits = sortUniq $ orbits >>= \(x,y) -> [x,y]

calculateTotalOrbits :: [(String,String)] -> [String] -> Int
calculateTotalOrbits _ [] = 0
calculateTotalOrbits orbits (planet:planets) = (calculateTotalPlanetOrbits orbits orbits planet) + (calculateTotalOrbits orbits planets)

calculateTotalPlanetOrbits :: [(String,String)] -> [(String,String)] -> String -> Int
calculateTotalPlanetOrbits [] _ _ = 0
calculateTotalPlanetOrbits ((parent,orbiter):xs) allOrbits planet
  | (parent == "COM") && (orbiter == planet) = 1
  | orbiter == planet = 1 + (calculateTotalPlanetOrbits allOrbits allOrbits parent)
  | otherwise = calculateTotalPlanetOrbits xs allOrbits planet
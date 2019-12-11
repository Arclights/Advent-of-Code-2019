module Day1(part1, part2) where

part1 :: [Int] -> Int
part1 = sum . map calculateFuel

part2 :: [Int] -> Int
part2 = sum . map (calculateAdditionalFuel 0)

calculateAdditionalFuel :: Int -> Int -> Int
calculateAdditionalFuel totalMass newMass
  | additionalFuel < 1 = totalMass
  | otherwise = calculateAdditionalFuel (additionalFuel + totalMass) additionalFuel
  where additionalFuel = calculateFuel newMass

calculateFuel :: Int -> Int
calculateFuel mass = (div mass 3) - 2
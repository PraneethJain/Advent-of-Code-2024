module Day1

import Data.String

parsePair : String -> Maybe (Int, Int)
parsePair pair = do
    let p1 = span (/= ' ') pair
        p2 = span (== ' ') (snd p1)
    num1 <- parseInteger (fst p1)
    num2 <- parseInteger (snd p2)
    pure (num1, num2)

parseInput : String -> List (Int, Int)
parseInput str = do
    nums <- lines str
    maybe [] pure (parsePair nums)
  
part1 : (List Int, List Int) -> Int
part1 (xs, ys) = sum (zipWith (\x, y => abs (x - y)) (sort xs) (sort ys))

export
sol1 : String -> String
sol1 = show . part1 . unzip . parseInput

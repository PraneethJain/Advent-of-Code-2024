module Day2

import Data.String

parseList : String -> List Int
parseList str = mapMaybe parseInteger $ words str

parseInput : String -> List (List Int)
parseInput str = map parseList $ lines str

isIncreasing : List Int -> Bool
isIncreasing [] = True
isIncreasing [_] = True
isIncreasing (x::y::xs) = x < y && (y - x <= 3) && (y - x >= 1) && isIncreasing (y::xs)

isDecreasing : List Int -> Bool
isDecreasing [] = True
isDecreasing [_] = True
isDecreasing (x::y::xs) = x > y && (x - y <= 3) && (x - y >= 1) && isDecreasing (y::xs)

isMonotonic : List Int -> Bool
isMonotonic xs = isIncreasing xs || isDecreasing xs

part1 : List (List Int) -> Nat
part1 arrs = length $ filter isMonotonic arrs

export
sol1 : String -> String
sol1 = show . part1 . parseInput


removeAt : Nat -> List a -> List a
removeAt _ [] = []
removeAt 0 (_::xs) = xs
removeAt (S k) (x::xs) = x :: removeAt k xs

canBecomeMonotonic : List Int -> Bool
canBecomeMonotonic xs = 
    let indices = map fromInteger [0..(cast (length xs) - 1)]
        removedLists = map (\i => removeAt i xs) indices
    in any isIncreasing removedLists || any isDecreasing removedLists

part2 : List (List Int) -> Nat
part2 arrs = length $ filter canBecomeMonotonic arrs

export
sol2 : String -> String
sol2 = show . part2 . parseInput
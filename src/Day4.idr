module Day4

import Data.String
import Data.List

parseInput : String -> List (List Char)
parseInput str = map unpack $ lines str

at : Nat -> List (List Char) -> Maybe (List Char)
at k xs = case drop k xs of
    [] => Nothing
    (y::ys) => Just y

att : Nat -> List Char -> Maybe Char
att k xs = case drop k xs of
    [] => Nothing
    (y::ys) => Just y

directions : List (Integer, Integer)
directions = [
    (0, 1),
    (1, 0),
    (0, -1),
    (-1, 0),
    (1, 1),
    (1, -1),
    (-1, -1),
    (-1, 1)
]

inBounds : List (List Char) -> (Integer, Integer) -> Maybe (Nat, Nat)
inBounds grid (x, y) =
    case grid of
        [] => Nothing
        (row1::_) => 
            if x >= 0 && y >= 0 && 
                x < cast (length grid) &&
                y < cast (length row1)
            then
                let
                    xNat = cast x
                    yNat = cast y
                in
                    Just (xNat, yNat)
            else
                Nothing

getSequence : List (List Char) -> (Integer, Integer) -> (Integer, Integer) -> List Char
getSequence grid (x, y) (dx, dy) = 
    let 
        coords = iterateN 4 (\(r, c) => (r + dx, c + dy)) (x, y)
        validCoords = mapMaybe (\(r, c) => inBounds grid (r, c)) coords
        sequence = mapMaybe (\(r, c) =>
            do
                row <- at r grid
                ch <- att c row
                pure ch
            ) validCoords
    in 
        sequence

checkSequence : List Char -> Bool
checkSequence sq = sq == ['X', 'M', 'A', 'S']

applyDirections: List (List Char) -> (Integer, Integer) -> Nat
applyDirections grid (x, y) = count checkSequence $ map (getSequence grid (x, y)) directions

part1 : List (List Char) -> Nat
part1 grid =
    case grid of
        [] => 0
        (row1::_) => 
            let rows = length grid
                cols = length (row1)
                points = case (rows, cols) of
                    (S rowsm1, S colsm1) =>  [(r, c) | r <- [0..rowsm1], c <- [0..colsm1]]
                    _ => []
            in sum $ map (\(r, c) => applyDirections grid (cast r, cast c)) points

export
sol1 : String -> String
sol1 = show . part1 . parseInput


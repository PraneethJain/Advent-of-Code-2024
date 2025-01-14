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

directions : List (Int, Int)
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

inBounds : List (List Char) -> (Int, Int) -> Maybe (Nat, Nat)
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

getSequence : List (List Char) -> (Int, Int) -> (Int, Int) -> List Char
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

applyDirections: List (List Char) -> (Int, Int) -> Nat
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

getSquare : List (List Char) -> (Int, Int) -> List Char
getSquare grid (x, y) =
    let
        coords = [(x - 1, y - 1), (x + 1, y - 1), (x + 1, y + 1), (x - 1, y + 1)]
        validCoords = mapMaybe (\(r, c) => inBounds grid (r, c)) coords
        sequence = mapMaybe (\(r, c) =>
            do
                row <- at r grid
                ch <- att c row
                pure ch
            ) validCoords
    in
        sequence

checkSquare : List Char -> Bool
checkSquare sq =
    sq == ['S', 'M', 'M', 'S'] ||
    sq == ['M', 'M', 'S', 'S'] ||
    sq == ['M', 'S', 'S', 'M'] ||
    sq == ['S', 'S', 'M', 'M']
                

checkX : List (List Char) -> (Nat, Nat) -> Maybe ()
checkX grid (x, y) =
    do
        row <- at x grid
        ch <- att y row
        if ch == 'A' && checkSquare (getSquare grid ((cast x), (cast y))) 
            then Just () 
            else Nothing

part2 : List (List Char) -> Nat
part2 grid =
    case grid of
        [] => 0
        (row1::_) => 
            let rows = length grid
                cols = length (row1)
                points = case (rows, cols) of
                    (S rowsm1, S colsm1) =>  [(r, c) | r <- [0..rowsm1], c <- [0..colsm1]]
                    _ => []
            in length $ mapMaybe (\(r, c) => checkX grid (cast r, cast c)) points

export
sol2 : String -> String
sol2 = show . part2 . parseInput
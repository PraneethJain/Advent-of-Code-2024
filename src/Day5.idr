module Day5

import Data.Nat
import Data.String
import Data.List1

at : Nat -> List (List Int) -> Maybe (List Int)
at k xs = case drop k xs of
    [] => Nothing
    (y::ys) => Just y

att : Nat -> List Int -> Maybe Int
att k xs = case drop k xs of
    [] => Nothing
    (y::ys) => Just y

Rules = List (Int, Int)
Updates = List (List Int)

parseRule : String -> Maybe (Int, Int)
parseRule rule = do
    x1 <- parseInteger $ strSubstr 0 2 rule
    x2 <- parseInteger $ strSubstr 3 5 rule
    pure (x1, x2)

parseRules : String -> Rules
parseRules str = mapMaybe parseRule $ takeWhile (/= "") (lines str)

parseUpdate : String -> List Int
parseUpdate update = mapMaybe parseInteger $ toList $ split (==',') update

parseUpdates : String -> Updates
parseUpdates str = map parseUpdate $ drop 1 $ dropWhile (/= "") (lines str)

parseInput : String -> (Rules, Updates)
parseInput str = (parseRules str, parseUpdates str)

checkPairInOrder : Rules -> (Int, Int) -> Bool
checkPairInOrder rules (a, b) = 
    case find (\(x, y) => x == b && y == a) rules of
        Just _ => False
        Nothing => True

checkPairOutOfOrder : Rules -> (Int, Int) -> Bool
checkPairOutOfOrder rules (a, b) = not $ checkPairInOrder rules (a, b)

pairwise : List a -> List (a, a)
pairwise [] = []
pairwise (x :: xs) = 
  let pairs = map (\y => (x, y)) xs
   in pairs ++ pairwise xs

checkUpdateInOrder : Rules -> List Int -> Bool
checkUpdateInOrder rules update = all (checkPairInOrder rules) $ pairwise update

checkUpdateOutOfOrder : Rules -> List Int -> Bool
checkUpdateOutOfOrder rules update = any (checkPairOutOfOrder rules) $ pairwise update

findMiddle : List Int -> Maybe Int
findMiddle xs = 
    let len = length xs
    in att (div len 2) xs

part1 : (Rules, Updates) -> Int
part1 (rules, updates) = sum $ mapMaybe findMiddle $ filter (checkUpdateInOrder rules) updates

export
sol1 : String -> String
sol1 = show . part1 . parseInput

sortByRules : Rules -> List Int -> List Int
sortByRules rules update = sortBy (\a, b => if (checkPairInOrder rules (a, b)) then LT else GT ) update

part2 : (Rules, Updates) -> Int
part2 (rules, updates) = sum $ mapMaybe findMiddle $ map (sortByRules rules) $ filter (checkUpdateOutOfOrder rules) updates

export
sol2 : String -> String
sol2 = show . part2 . parseInput

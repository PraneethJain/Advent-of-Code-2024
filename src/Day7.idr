module Day7

import Data.String

trimColonAndSpaces : String -> String
trimColonAndSpaces s = pack (dropWhile (\c => c == ':' || isSpace c) (unpack s))

parseLine : String -> Maybe (Integer, List Integer)
parseLine line =
    case break (== ':') line of
        (targetStr, rest) => do
            target <- parseInteger (trim targetStr)
            nums <- traverse parseInteger (words (trimColonAndSpaces rest))
            pure (target, nums)

parseInput : String -> List (Integer, List Integer)
parseInput input = mapMaybe parseLine (lines input)

opsPart1 : List (Integer -> Integer -> Integer)
opsPart1 = [ (+), (*) ]

concatOp : Integer -> Integer -> Integer
concatOp a b = cast (cast {to=String} a ++ cast {to=String} b)

opsPart2 : List (Integer -> Integer -> Integer)
opsPart2 = [ (+), (*), concatOp ]

allOpCombos : List (Integer -> Integer -> Integer) -> Nat -> List (List (Integer -> Integer -> Integer))
allOpCombos ops Z = [[]]
allOpCombos ops (S k) = [op :: rest | op <- ops, rest <- allOpCombos ops k]

evalOps : List (Integer -> Integer -> Integer) -> List Integer -> Integer
evalOps [] (n :: _) = n
evalOps (op :: ops) (n1 :: n2 :: ns) = evalOps ops ((op n1 n2) :: ns)
evalOps _ _ = 0

canMakeTarget : List (Integer -> Integer -> Integer) -> Integer -> List Integer -> Bool
canMakeTarget ops target nums =
    case nums of
        [] => False
        [_] => False
        _ => let nOpsNeeded = minus (length nums) 1 in
            any (\opsList => evalOps opsList nums == target)
                (allOpCombos ops nOpsNeeded)

solve : List (Integer -> Integer -> Integer) -> List (Integer, List Integer) -> Integer
solve ops eqns = sum [t | (t, nums) <- eqns, canMakeTarget ops t nums]

part1 : List (Integer, List Integer) -> Integer
part1 = solve opsPart1

export
sol1 : String -> String
sol1 = show . part1 . parseInput

part2 : List (Integer, List Integer) -> Integer
part2 = solve opsPart2

export
sol2 : String -> String
sol2 = show . part2 . parseInput
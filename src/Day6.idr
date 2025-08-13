module Day6

import Data.String
import Data.SortedSet

data Dir = Up | Right | Down | Left

record Pos where
    constructor MkPos
    row : Int
    col : Int

implementation Eq Pos where
    (MkPos r1 c1) == (MkPos r2 c2) = r1 == r2 && c1 == c2

implementation Ord Pos where
    compare (MkPos r1 c1) (MkPos r2 c2) =
    case compare r1 r2 of
        EQ => compare c1 c2
        x  => x

record Guard where
    constructor MkGuard
    pos : Pos
    dir : Dir

record MapInfo where
    constructor MkMapInfo
    obstacles : SortedSet Pos
    width : Nat
    height : Nat
    startGuard : Guard

parseInput : String -> MapInfo
parseInput input =
    let ls = lines input
        h = length ls
        w = maybe 0 (length . unpack) (head' ls)
        indexedLines = zip (map cast [0 .. minus h 1]) ls
        obstacles =
            fromList [ MkPos r c
                    | (r, line) <- indexedLines
                    , (c, char) <- zip (map cast [0 .. minus w 1]) (unpack line)
                    , char == '#'
                    ]
    in MkMapInfo obstacles (cast w) (cast h) $ MkGuard (MkPos 39 46) Up

turnRight : Dir -> Dir
turnRight Up    = Right
turnRight Right = Down
turnRight Down  = Left
turnRight Left  = Up

moveForward : Pos -> Dir -> Pos
moveForward (MkPos r c) Up    = MkPos (r - 1) c
moveForward (MkPos r c) Right = MkPos r (c + 1)
moveForward (MkPos r c) Down  = MkPos (r + 1) c
moveForward (MkPos r c) Left  = MkPos r (c - 1)

isOutOfBounds : Pos -> Nat -> Nat -> Bool
isOutOfBounds (MkPos r c) height width =
    r < 0 || c < 0 || cast r >= height || cast c >= width

step : Guard -> MapInfo -> Guard
step guard mapInfo =
    let frontPos = moveForward guard.pos guard.dir
    in
        if contains frontPos mapInfo.obstacles
            then MkGuard guard.pos (turnRight guard.dir)
        else MkGuard frontPos guard.dir

runSimulation : Guard -> MapInfo -> SortedSet Pos -> SortedSet Pos
runSimulation guard mapInfo visited =
    if isOutOfBounds guard.pos mapInfo.height mapInfo.width
        then visited
    else
        let newVisited = insert guard.pos visited
            nextGuard = step guard mapInfo
        in runSimulation nextGuard mapInfo newVisited

part1 : MapInfo -> Nat
part1 mapInfo = length $ Prelude.toList $ runSimulation mapInfo.startGuard mapInfo empty

export
sol1 : String -> String
sol1 = show . part1 . parseInput
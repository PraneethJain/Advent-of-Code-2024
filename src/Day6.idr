module Day6

import Data.String
import Data.SortedSet

data Dir = Up | Right | Down | Left

dirCode : Dir -> Int
dirCode Up    = 0
dirCode Right = 1
dirCode Down  = 2
dirCode Left  = 3

implementation Eq Dir where
  Up == Up = True
  Right == Right = True
  Down == Down = True
  Left == Left = True
  _ == _ = False

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

record State where
    constructor MkState
    pos : Pos
    dir : Dir

implementation Eq State where
    (MkState p1 d1) == (MkState p2 d2) = p1 == p2 && d1 == d2

implementation Ord State where
    compare (MkState p1 d1) (MkState p2 d2) =
        case compare p1 p2 of
            EQ => compare (dirCode d1) (dirCode d2)
            x  => x

record MapInfo where
    constructor MkMapInfo
    obstacles : SortedSet Pos
    width : Nat
    height : Nat
    startGuard : State

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
    in MkMapInfo obstacles (cast w) (cast h) $ MkState (MkPos 39 46) Up

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

isBlocked : Pos -> MapInfo -> Maybe Pos -> Bool
isBlocked p mi extra = contains p mi.obstacles || maybe False (== p) extra

stepWith : State -> MapInfo -> Maybe Pos -> State
stepWith guard mi extra =
    let frontPos = moveForward guard.pos guard.dir
    in
        if isBlocked frontPos mi extra
            then MkState guard.pos (turnRight guard.dir)
        else MkState frontPos guard.dir

runSimulation : State -> MapInfo -> SortedSet Pos -> SortedSet Pos
runSimulation guard mapInfo visited =
    if isOutOfBounds guard.pos mapInfo.height mapInfo.width
        then visited
    else
        let newVisited = insert guard.pos visited
            nextGuard = stepWith guard mapInfo Nothing
        in runSimulation nextGuard mapInfo newVisited

willLoop : State -> MapInfo -> Maybe Pos -> SortedSet State -> Bool
willLoop guard mi extra seen =
    if isOutOfBounds guard.pos mi.height mi.width then False
    else
        let st = MkState guard.pos guard.dir in
            if contains st seen then True
            else willLoop (stepWith guard mi extra) mi extra (insert st seen)

candidates : MapInfo -> List Pos
candidates mi =
    filter
        (\p => not (p == mi.startGuard.pos) && not (contains p mi.obstacles))
        $ Prelude.toList $ runSimulation mi.startGuard mi empty

part1 : MapInfo -> Nat
part1 mapInfo = length $ Prelude.toList $ runSimulation mapInfo.startGuard mapInfo empty

export
sol1 : String -> String
sol1 = show . part1 . parseInput

part2 : MapInfo -> Nat
part2 mi =
    length $ filter
                (\p => willLoop mi.startGuard mi (Just p) empty)
                $ candidates mi

export
sol2 : String -> String
sol2 = show . part2 . parseInput
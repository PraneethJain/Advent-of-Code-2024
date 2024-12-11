module Main

import Day1
import Day2
import Day4
import Day5
import System
import System.File
import Data.String

runDay : Int -> String -> String
runDay n inp = case n of
  1 => unlines [Day1.sol1 inp, Day1.sol2 inp]
  2 => unlines [Day2.sol1 inp, Day2.sol2 inp]
  4 => unlines [Day4.sol1 inp, Day4.sol2 inp]
  5 => Day5.sol1 inp
  _ => "Not implemented yet!"

main : IO ()
main = do
  einp <- readFile ("./data/day5.txt")
  inp <- either (const $ die "Not implemented yet!") pure einp
  putStrLn $ runDay 5 inp

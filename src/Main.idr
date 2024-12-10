module Main

import Day1
import Day2
import System
import System.File
import Data.String

runDay : Int -> String -> String
runDay n inp = case n of
  1 => unlines [Day1.sol1 inp, Day1.sol2 inp]
  2 => Day2.sol1 inp
  _ => "Not implemented yet!"

main : IO ()
main = do
  einp <- readFile ("./data/day2.txt")
  inp <- either (const $ die "Not implemented yet!") pure einp
  putStrLn $ runDay 2 inp

module Main

import Day1
import System
import System.File
import Data.String

runDay : Int -> String -> String
runDay n inp = case n of
  1 => unlines [Day1.sol1 inp, Day1.sol2 inp]
  _ => "Not implemented yet!"

main : IO ()
main = do
  einp <- readFile ("./data/day1.txt")
  inp <- either (const $ die "Not implemented yet!") pure einp
  putStrLn $ runDay 1 inp

import System.IO
import Data.Char

toInt ('-':s) = (-(toInt_ (reverse (filter (\x -> x >= '0' && x <= '9') s))))
toInt s = toInt_ (reverse (filter (\x -> x >= '0' && x <= '9') s))
toInt_ (c:[]) = digitToInt c
toInt_ (c:rest) = (digitToInt c) + 10 * (toInt_ rest)

getFuel mass = if fuel <= 0
  then 0
  else fuel + (getFuel fuel)
  where fuel = fromIntegral ((floor (mass / 3)) - 2) 

main :: IO ()
main = do
   helloFile <- openFile "1.txt" ReadMode
   getContents <- hGetContents helloFile
   let nums = map toInt (lines getContents)
   let floats = map fromIntegral nums
   let fuel = map (\x -> (floor (x / 3)) - 2) floats
   putStrLn ("Part 1: " ++ (show (sum fuel)))
   let recursiveFuel = map getFuel floats
   putStrLn ("Part 2: " ++ (show (sum recursiveFuel)))
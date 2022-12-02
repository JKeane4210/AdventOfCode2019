import System.IO
import Data.Char
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Map as M
import Debug.Trace

toInt ('-':s) = (-(toInt_ (reverse (filter (\x -> x >= '0' && x <= '9') s))))
toInt s = toInt_ (reverse (filter (\x -> x >= '0' && x <= '9') s))
toInt_ (c:[]) = digitToInt c
toInt_ (c:rest) = (digitToInt c) + 10 * (toInt_ rest)

points x y steps (('R':num):moves) = [((x + i, y), steps + i) | i <- [1..n]] ++ (points (x + n) y (steps + n) moves)
  where n = (toInt num)
points x y steps (('L':num):moves) = [((x - i, y), steps + i) | i <- [1..n]] ++ (points (x - n) y (steps + n) moves)
  where n = (toInt num)
points x y steps (('U':num):moves) = [((x, y + i), steps + i) | i <- [1..n]] ++ (points x (y + n) (steps + n) moves)
  where n = (toInt num)
points x y steps (('D':num):moves) = [((x, y - i), steps + i) | i <- [1..n]] ++ (points x (y - n) (steps + n) moves)
  where n = (toInt num)
points x y steps [] = []

minManhattan (point:[]) = ((abs (fst (fst point))) + (abs (snd (fst point))))
minManhattan (point:points) = min ((abs (fst (fst point))) + (abs (snd (fst point)))) (minManhattan points)

minSteps (point:[]) = snd point
minSteps (point:points) = min (snd point) (minSteps points)

-- function for combining keys in a map intersection
combine key left right = left + right

main :: IO ()
main = do
   helloFile <- openFile "3.txt" ReadMode
   getContents <- hGetContents helloFile
   let wires = lines getContents
   let splitWires = [map T.unpack (T.splitOn (T.pack ",") (T.pack wire)) | wire <- wires]
   let wirePoints = [(M.fromList (points 0 0 0 wire)) | wire <- splitWires]
   let intersect = M.toList (M.intersectionWithKey combine (wirePoints !! 0) (wirePoints !! 1))
   putStrLn ("Part 1: " ++ (show (minManhattan intersect)))
   putStrLn ("Part 2: " ++ (show (minSteps intersect)))
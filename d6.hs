import System.IO
import Data.Char
import qualified Data.Text as T
import Debug.Trace

type ParentName = String
type Name = String
data OrbitObject = Object ParentName Name | None deriving (Show, Eq)

toObject obj = let pieces = map T.unpack (T.splitOn (T.pack ")") (T.pack obj))
  in Object (pieces !! 0) (pieces !! 1)

find name ((Object p n):rest) = if name == n then (Object p n) else find name rest
find name [] = None

countAllOrbits objects (obj:rest) = (countOrbits objects obj) + (countAllOrbits objects rest)
countAllOrbits objects [] = 0

countOrbits :: (Num a) => [OrbitObject] -> OrbitObject -> a
countOrbits objects (Object p n) =
  let parent = (find p objects) in (parentValue parent) + (countOrbits objects parent)
countOrbits objects (None) = 0

path objects (Object p n) =
  let parent = (find p objects) in ((Object p n):(path objects parent))
path objects (None) = []

parentValue None = 1
parentValue (Object p n) = 1

main :: IO ()
main = do
   helloFile <- openFile "6.txt" ReadMode
   getContents <- hGetContents helloFile
   let objs = lines getContents
   let objects = [toObject obj | obj <- objs]
   let you = find "YOU" objects
   let san = find "SAN" objects
   let youp = path objects you
   let sanp = path objects san
   let overlap = [x | x <- youp, y <- sanp, x == y]
   let part2 = let overlapl = length overlap in (length youp) - overlapl + (length sanp) - overlapl - 2
   putStrLn ("Part 1: " ++ show (countAllOrbits objects objects))
   putStrLn ("Part 2: " ++ (show part2))
   

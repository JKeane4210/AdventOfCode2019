import System.IO
import Data.Char
import qualified Data.Text as T

type ParentName = String
type Name = String
data OrbitObject deriving Show = Object ParentName Name 

toObject obj = let pieces = map T.unpack (T.splitOn (T.pack ")") (T.pack obj))
  in Object (pieces !! 0) (pieces !! 1)

main :: IO ()
main = do
   helloFile <- openFile "6.txt" ReadMode
   getContents <- hGetContents helloFile
   let objs = lines getContents
   let objects = [toObject obj | obj <- objs]
   putStrLn (show objects)

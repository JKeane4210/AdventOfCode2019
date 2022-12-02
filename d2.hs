import System.IO
import Data.Char
import qualified Data.Text as T
import Debug.Trace

toInt ('-':s) = (-(toInt_ (reverse (filter (\x -> x >= '0' && x <= '9') s))))
toInt s = toInt_ (reverse (filter (\x -> x >= '0' && x <= '9') s))
toInt_ (c:[]) = digitToInt c
toInt_ (c:rest) = (digitToInt c) + 10 * (toInt_ rest)

lastN n xs l = drop (l - n) xs

replace i l v = (take (i) l) ++ [v] ++ (drop (i + 1) l)

eval vals n (1:x:y:z:rest) = let next = replace z vals ((vals !! x) + (vals !! y)) 
  -- in trace ("Add: " ++ (show x) ++ "=" ++ (show (vals !! x)) ++ " " ++ (show y) ++ "=" ++ (show (vals !! y)) ++ " " ++ (show z)) (eval next n (lastN (length rest) next n))
  in (eval next n (lastN (length rest) next n))
eval vals n (2:x:y:z:rest) = let next = replace z vals ((vals !! x) * (vals !! y)) 
  -- in trace ("Multiply: " ++ (show x) ++ "=" ++ (show (vals !! x)) ++ " " ++ (show y) ++ "=" ++ (show (vals !! y)) ++ " " ++ (show z)) (eval next n (lastN (length rest) next n))
  in (eval next n (lastN (length rest) next n))
eval vals n (99:rest) = vals
eval vals n rest = trace ("Invalid: " ++ (show (rest))) [-1]

input_noun_verb nums noun verb = let new_nums = ((head nums):noun:verb:(drop 3 nums)) in
  head (eval new_nums (length nums) new_nums)

pt2 nums = let n = length nums in 
  filter (\t -> snd t == 19690720) [((noun * 100 + verb, (input_noun_verb nums noun verb))) | noun <- [0..99], verb <- [0..99]]

main :: IO ()
main = do
   helloFile <- openFile "2.txt" ReadMode
   getContents <- hGetContents helloFile
   -- getLine <- hGetLine helloFile
   let contents = map T.unpack (T.splitOn (T.pack ",") (T.pack getContents))
   let nums = map toInt contents
   putStrLn ("Part 1: " ++ show (input_noun_verb nums 12 2))
   putStrLn ("Part 2: " ++ show (fst (head (pt2 nums))))



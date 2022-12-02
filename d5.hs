import System.IO
import Data.Char
import qualified Data.Text as T
import Debug.Trace
import Data.Typeable

toInt ('-':s) = (-(toInt_ (reverse (filter (\x -> x >= '0' && x <= '9') s))))
toInt s = toInt_ (reverse (filter (\x -> x >= '0' && x <= '9') s))
toInt_ (c:[]) = digitToInt c
toInt_ (c:rest) = (digitToInt c) + 10 * (toInt_ rest)

lastN n xs l = drop (l - n) xs

replace i l v = (take (i) l) ++ [v] ++ (drop (i + 1) l)

eval vals n (1:x:y:z:rest) inp = let next = replace z vals ((vals !! x) + (vals !! y)) 
  in (eval next n (lastN (length rest) next n)) inp
eval vals n (2:x:y:z:rest) inp = let next = replace z vals ((vals !! x) * (vals !! y)) 
  in (eval next n (lastN (length rest) next n)) inp
eval vals n (99:rest) inp = vals
eval vals n (opcode:rest) inp = (evalParamMode vals n (reverse (show opcode)) rest inp)

evalParamMode vals n ('1':'0':params) (x:y:z:rest) inp = let next = replace z vals ((getParam vals x (getParamMode 0 params)) + (getParam vals y (getParamMode 1 params)))
  in (eval next n (lastN (length rest) next n)) inp
evalParamMode vals n ('2':'0':params) (x:y:z:rest) inp = let next = replace z vals ((getParam vals x (getParamMode 0 params)) * (getParam vals y (getParamMode 1 params)))
  in (eval next n (lastN (length rest) next n)) inp
evalParamMode vals n ('3':params) (x:rest) inp = let next = replace x vals inp 
  in (eval next n (lastN (length rest) next n) inp)
evalParamMode vals n ('4':params) (x:rest) inp = trace ("Diagnostic: " ++ show (getParam vals x (getParamMode 1 params))) (eval vals n rest inp)
evalParamMode vals n ('5':params) (x:y:rest) inp = if (getParam vals x (getParamMode 1 params)) /= 0
  then (eval vals n (drop (getParam vals y (getParamMode 2 params)) vals) inp)
  else (eval vals n rest inp)
evalParamMode vals n ('6':params) (x:y:rest) inp = if (getParam vals x (getParamMode 1 params)) == 0
  then (eval vals n (drop (getParam vals y (getParamMode 2 params)) vals) inp)
  else (eval vals n rest inp)
evalParamMode vals n ('7':params) (x:y:z:rest) inp = 
  let store = if (getParam vals x (getParamMode 1 params)) < (getParam vals y (getParamMode 2 params)) then 1 else 0
  in let next = replace z vals store
     in (eval next n (lastN (length rest) next n) inp)
evalParamMode vals n ('8':params) (x:y:z:rest) inp = 
  let store = if (getParam vals x (getParamMode 1 params)) == (getParam vals y (getParamMode 2 params)) then 1 else 0
  in let next = replace z vals store
     in (eval next n (lastN (length rest) next n) inp)

getParamMode i params = if i < (length params) then (params !! i) else '0'

getParam vals i paramCode = if paramCode == '1' then i else (vals !! i)

main :: IO ()
main = do
   helloFile <- openFile "5.txt" ReadMode
   getContents <- hGetContents helloFile
   let contents = map T.unpack (T.splitOn (T.pack ",") (T.pack getContents))
   let nums = map toInt contents
   putStrLn ("\nPart 1:\n" ++ show (eval nums (length nums) nums 1))
   putStrLn ("\nPart 2:\n" ++ show (eval nums (length nums) nums 5))

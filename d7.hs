import System.IO
import Data.Char
import qualified Data.Text as T
import qualified Data.Set as S
import Debug.Trace
import Data.Typeable

(%) a b = a `mod` b

toInt ('-':s) = (-(toInt_ (reverse (filter (\x -> x >= '0' && x <= '9') s))))
toInt s = toInt_ (reverse (filter (\x -> x >= '0' && x <= '9') s))
toInt_ (c:[]) = digitToInt c
toInt_ (c:rest) = (digitToInt c) + 10 * (toInt_ rest)

lastN n xs l = drop (l - n) xs

replace i l v = (take (i) l) ++ [v] ++ (drop (i + 1) l)

eval vals n (99:rest) inp out = (out, vals, (99:rest), 99)
eval vals n (opcode:rest) inp out = (evalParamMode vals n (reverse (show opcode)) rest inp out)

evalParamMode vals n ('1':params) (x:y:z:rest) inp out = let next = replace z vals ((getParam vals x (getParamMode 1 params)) + (getParam vals y (getParamMode 2 params)))
  in (eval next n (lastN (length rest) next n) inp out)
evalParamMode vals n ('2':params) (x:y:z:rest) inp out = let next = replace z vals ((getParam vals x (getParamMode 1 params)) * (getParam vals y (getParamMode 2 params)))
  in (eval next n (lastN (length rest) next n) inp out)
evalParamMode vals n ('3':params) (x:rest) [] out = (out, vals, ((toInt (reverse ('3':params))):x:rest), 3) -- trace "EMPTY"
evalParamMode vals n ('3':params) (x:rest) inp out = let next = replace x vals (head inp)
  in  (eval next n (lastN (length rest) next n) (tail inp) out) -- trace ("IN " ++ show inp) 
evalParamMode vals n ('4':params) (x:rest) inp out = let diag = (getParam vals x (getParamMode 1 params)) 
  in (eval vals n rest inp (out ++ [diag])) -- trace ("OUT " ++ show (out ++ [diag]))
evalParamMode vals n ('5':params) (x:y:rest) inp out = if (getParam vals x (getParamMode 1 params)) /= 0
  then (eval vals n (drop (getParam vals y (getParamMode 2 params)) vals) inp out)
  else (eval vals n rest inp out)
evalParamMode vals n ('6':params) (x:y:rest) inp out = if (getParam vals x (getParamMode 1 params)) == 0
  then (eval vals n (drop (getParam vals y (getParamMode 2 params)) vals) inp out)
  else (eval vals n rest inp out)
evalParamMode vals n ('7':params) (x:y:z:rest) inp out = 
  let store = if (getParam vals x (getParamMode 1 params)) < (getParam vals y (getParamMode 2 params)) then 1 else 0
  in let next = replace z vals store
     in (eval next n (lastN (length rest) next n) inp out)
evalParamMode vals n ('8':params) (x:y:z:rest) inp out = 
  let store = if (getParam vals x (getParamMode 1 params)) == (getParam vals y (getParamMode 2 params)) then 1 else 0
  in let next = replace z vals store
     in (eval next n (lastN (length rest) next n) inp out)

getParamMode i params = if i < (length params) then (params !! i) else '0'

getParam vals i paramCode = if paramCode == '1' then i else (vals !! i)

-- rehashed to work with part 2 eval return
thrusterOut1 amps ins instrs codes outs i = 
  let (out, amp, currInstr, code) = eval (amps !! i) (length (amps !! i)) (instrs !! i) (ins !! i) []
      next = (i + 1)
  in (if i == 4 then last out else thrusterOut1 amps (replace next ins ((ins !! next) ++ [last out])) instrs codes outs next)

thrusterOut2 amps ins instrs codes outs i = 
  let (out, amp, currInstr, code) = eval (amps !! i) (length (amps !! i)) (instrs !! i) (ins !! i) []
      next = (i + 1) % 5
  in (if all99 codes -- trace (show i ++  ": " ++ (show (codes !! i)) ++ "->" ++ show code ++ "\n")
    then (outs !! 4)
    else (if (codes !! next) == 0
      then (thrusterOut2 (replace i amps amp) (replace next ins ((ins !! next) ++ out)) (replace i instrs currInstr) (replace i codes code) (replace i outs (last out)) next)
      else (thrusterOut2 (replace i amps amp) (replace next ins out) (replace i instrs currInstr) (replace i codes code) (replace i outs (last out)) next)))

all99 (code:codes) = code == 99 && (all99 codes)
all99 [] = True

maxSol nums = maxL [(thrusterOut1 [nums, nums, nums, nums, nums] [[a, 0], [b], [c], [d], [e]] [nums, nums, nums, nums, nums] [0, 0, 0, 0, 0] [0, 0, 0, 0, 0] 0) | 
  a <- [0..4], 
  b <- [0..4], 
  c <- [0..4], 
  d <- [0..4], 
  e <- [0..4], 
  (S.size (S.fromList [a, b, c, d, e])) == 5]

maxSol2 nums = maxL [(thrusterOut2 [nums, nums, nums, nums, nums] [[a, 0], [b], [c], [d], [e]] [nums, nums, nums, nums, nums] [0, 0, 0, 0, 0] [0, 0, 0, 0, 0] 0) | 
  a <- [5..9], 
  b <- [5..9], 
  c <- [5..9], 
  d <- [5..9], 
  e <- [5..9], 
  (S.size (S.fromList [a, b, c, d, e])) == 5]

maxL (x:[]) = x
maxL (x:rest) = max x (maxL rest)

main :: IO ()
main = do
   helloFile <- openFile "7.txt" ReadMode
   getContents <- hGetContents helloFile
   let contents = map T.unpack (T.splitOn (T.pack ",") (T.pack getContents))
   let nums = map toInt contents 
   putStrLn ("Part 1: " ++ (show (maxSol nums)))
   putStrLn ("Part 2: " ++ (show (maxSol2 nums)))

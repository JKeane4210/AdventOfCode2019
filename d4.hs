increasing (c1:[]) = True
increasing (c1:c2:s) = c1 <= c2 && (increasing (c2:s))

adjacent (c1:c2:s) = c1 == c2 || (adjacent (c2:s))
adjacent (c1:[]) = False

streak2 (c:s) curr count
  | (null s && c == curr) = (count + 1) == 2
  | (null s && c /= curr) = count == 2
  | (c /= curr) && (count == 2) = True
  | (c /= curr) = streak2 s c 1
  | (c == curr) = streak2 s curr (count + 1)

valid n = let s = show n
  in (adjacent s) && (increasing s)

valid2 n = let s = show n
  in (increasing s) && (streak2 s '-' 0)

search a b = [v | v <- [a..b], valid v]

search2 a b = [v | v <- [a..b], valid2 v]

main :: IO ()
main = do
    let minRange = 147981
    let maxRange = 691423
    putStrLn ("Part 1: " ++ show ((length (search minRange maxRange))))
    putStrLn ("Part 2: " ++ show ((length (search2 minRange maxRange))))
import Debug.Trace

q8_e = "e"

q8_dot x y | x == q8_e = y
q8_dot x y | y == q8_e = x
q8_dot x y | x == "s" && y == "s" = q8_e
q8_dot x y | x == "i" && y == "i" = "s"
q8_dot x y | x == "j" && y == "j" = "s"
q8_dot x y | x == "k" && y == "k" = "s"

q8_dot x y | x == "s" && y == "i" = "si"
q8_dot x y | x == "s" && y == "si" = "i"
q8_dot x y | x == "s" && y == "j" = "sj"
q8_dot x y | x == "s" && y == "sj" = "j"
q8_dot x y | x == "s" && y == "k" = "sk"
q8_dot x y | x == "s" && y == "sk" = "k"

q8_dot x y | x == "i" && y == "s" = "si"
q8_dot x y | x == "i" && y == "si" = q8_e
q8_dot x y | x == "i" && y == "j" = "k"
q8_dot x y | x == "i" && y == "sj" = "sk"
q8_dot x y | x == "i" && y == "k" = "sj"
q8_dot x y | x == "i" && y == "sk" = "j"

q8_dot x y | x == "si" && y == "s" = "i"
q8_dot x y | x == "si" && y == "i" = q8_e
q8_dot x y | x == "si" && y == "si" = "s"
q8_dot x y | x == "si" && y == "j" = "sk"
q8_dot x y | x == "si" && y == "sj" = "k"
q8_dot x y | x == "si" && y == "k" = "j"
q8_dot x y | x == "si" && y == "sk" = "sj"

q8_dot x y | x == "j" && y == "s" = "sj"
q8_dot x y | x == "j" && y == "i" = "sk"
q8_dot x y | x == "j" && y == "si" = "k"
q8_dot x y | x == "j" && y == "sj" = q8_e
q8_dot x y | x == "j" && y == "k" = "i"
q8_dot x y | x == "j" && y == "sk" = "si"

q8_dot x y | x == "sj" && y == "s" = "j"
q8_dot x y | x == "sj" && y == "i" = "k"
q8_dot x y | x == "sj" && y == "si" = "sk"
q8_dot x y | x == "sj" && y == "j" = "sk"
q8_dot x y | x == "sj" && y == "sj" = "s"
q8_dot x y | x == "sj" && y == "k" = "si"
q8_dot x y | x == "sj" && y == "sk" = "i"

q8_dot x y | x == "k" && y == "s" = "sk"
q8_dot x y | x == "k" && y == "i" = "j"
q8_dot x y | x == "k" && y == "si" = "sj"
q8_dot x y | x == "k" && y == "j" = "si"
q8_dot x y | x == "k" && y == "sj" = "i"
q8_dot x y | x == "k" && y == "sk" = q8_e

q8_dot x y | x == "sk" && y == "s" = "k"
q8_dot x y | x == "sk" && y == "i" = "sj"
q8_dot x y | x == "sk" && y == "si" = "j"
q8_dot x y | x == "sk" && y == "j" = "i"
q8_dot x y | x == "sk" && y == "sj" = "si"
q8_dot x y | x == "sk" && y == "k" = q8_e
q8_dot x y | x == "sk" && y == "sk" = "s"


q8_dot x y = trace ("DEBUG: x=" ++ show x ++ ", y=" ++ show y) "?"

q8 :: [String] -> [String]
q8 [] = []
q8 [x] = trace ("DEBUG1: x=" ++ show x) [x]
q8 (x:xs) = trace ("DEBUG1+: x=" ++ show x ++ ", xs=" ++ show xs ++ ", len=" ++ show (length xs)) q8 ([q8_dot x (head xs)] ++ (tail xs))

q8_inv x = head [x | y <- [q8_e, "s", "i", "si", "j", "sj", "k", "sk"], q8_dot x y == q8_e]

main = do
  putStrLn $ "i * j * k = " ++ head (q8 ["si", "i", "j", "k"])
  putStrLn $ "(si)^(-1) = " ++ q8_inv "si"

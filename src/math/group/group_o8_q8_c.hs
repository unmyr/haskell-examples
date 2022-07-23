import Debug.Trace

q8_e = "e"
q8_all = [q8_e, "s", "i", "si", "j", "sj", "k", "sk"]

q8_dot x y | x == q8_e = y
q8_dot x y | y == q8_e = x
q8_dot x y | x == "s" && y == "s" = q8_e
q8_dot x y | x == "i" && y == "i" = "s"
q8_dot x y | x == "j" && y == "j" = "s"
q8_dot x y | x == "k" && y == "k" = "s"

q8_dot x y | x == "s" && y == "i" = "si"
q8_dot x y | x == "s" && y == "j" = "sj"
q8_dot x y | x == "s" && y == "k" = "sk"

q8_dot x y | x == "i" && y == "j" = "k"
q8_dot x y | x == "i" && y == "k" = "sj"

q8_dot x y | x == "j" && y == "i" = "sk"
q8_dot x y | x == "j" && y == "k" = "i"

q8_dot x y | x == "k" && y == "i" = "j"
q8_dot x y | x == "k" && y == "j" = "si"

-- `s` is element of the center of a group Q8
q8_dot x s       | x /= "s" && s == "s" = q8_dot s x
q8_dot x (s:ys)  | x /= "s" && s == 's' = q8_dot [s] (q8_dot x ys)
q8_dot s (s2:ys) | s == "s" && s2 == 's' = ys
q8_dot (s:xs) y  | s == 's' = q8_dot [s] (q8_dot xs y)

q8_dot x y = trace ("DEBUG: x=" ++ show x ++ ", y=" ++ show y) "?"

q8 :: [String] -> [String]
q8 [] = []
q8 [x] = [x]
q8 (x:xs) = q8 ([q8_dot x (head xs)] ++ (tail xs))

q8_inv x = head [x | y <- q8_all, q8_dot x y == q8_e]

main = do
  print $ [q8_dot "e"  y | y <- q8_all] == [ "e", "s", "i","si", "j","sj", "k","sk"]
  print $ [q8_dot "s"  y | y <- q8_all] == [ "s", "e","si", "i","sj", "j","sk", "k"]
  print $ [q8_dot "i"  y | y <- q8_all] == [ "i","si", "s", "e", "k","sk","sj", "j"]
  print $ [q8_dot "si" y | y <- q8_all] == ["si", "i", "e", "s","sk", "k", "j","sj"]
  print $ [q8_dot "j"  y | y <- q8_all] == [ "j","sj","sk", "k", "s", "e", "i","si"]
  print $ [q8_dot "sj" y | y <- q8_all] == ["sj", "j", "k","sk", "e", "s","si", "i"]
  print $ [q8_dot "k"  y | y <- q8_all] == [ "k","sk", "j","sj","si", "i", "s", "e"]
  print $ [q8_dot "sk" y | y <- q8_all] == ["sk", "k","sj", "j", "i","si", "e", "s"]
  putStrLn $ "si * si = " ++ head (q8 ["si", "si"])
  putStrLn $ " i * sk = " ++ head (q8 ["i", "sj"])
  putStrLn $ "si * si  = " ++ head (q8 ["si", "si"])
  putStrLn $ "si * i * j * k = " ++ head (q8 ["si", "i", "j", "k"])
  putStrLn $ "(si)^(-1) = " ++ q8_inv "si"

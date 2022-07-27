import Debug.Trace

d3_e :: String
d3_e = "e"

d3_all :: [String]
d3_all = [d3_e, "r", "rr", "s", "sr", "srr"]

d3_dot :: String -> String -> String
d3_dot x y | x == d3_e = y
d3_dot x y | y == d3_e = x

d3_dot x y | x == "r" && y == "r" = "rr"
d3_dot x y | x == "r" && y == "rr" = d3_e
d3_dot x y | x == "r" && y == "s" = "srr"
d3_dot x y | x == "r" && y == "sr" = "s"
d3_dot x y | x == "r" && y == "srr" = "sr"

d3_dot x y | x == "rr" && y == "r" = d3_e
d3_dot x y | x == "rr" && y == "rr" = "r"
d3_dot x y | x == "rr" && y == "s" = "sr"
d3_dot x y | x == "rr" && y == "sr" = "srr"
d3_dot x y | x == "rr" && y == "srr" = "s"

d3_dot x y | x == "s" && y == "r" = "sr"
d3_dot x y | x == "s" && y == "rr" = "srr"
d3_dot x y | x == "s" && y == "s" = d3_e
d3_dot x y | x == "s" && y == "sr" = "r"
d3_dot x y | x == "s" && y == "srr" = "rr"

d3_dot x y | x == "sr" && y == "r" = "srr"
d3_dot x y | x == "sr" && y == "rr" = "s"
d3_dot x y | x == "sr" && y == "s" = "rr"
d3_dot x y | x == "sr" && y == "sr" = d3_e
d3_dot x y | x == "sr" && y == "srr" = "r"

d3_dot x y | x == "srr" && y == "r" = "s"
d3_dot x y | x == "srr" && y == "rr" = "sr"
d3_dot x y | x == "srr" && y == "s" = "r"
d3_dot x y | x == "srr" && y == "sr" = "rr"
d3_dot x y | x == "srr" && y == "srr" = d3_e

d3_dot x y = trace ("DEBUG: x=" ++ show x ++ ", y=" ++ show y) "?"

d3 :: [String] -> [String]
d3 [] = []
d3 [x] = [x]
d3 (x:xs) = d3 ([d3_dot x (head xs)] ++ (tail xs))

d3_inv :: String -> String
d3_inv x = head [x | y <- d3_all, d3_dot x y == d3_e]

main :: IO ()
main = do
  print $ [d3_dot "e"    y | y <- d3_all] == [  "e",  "r", "rr",  "s", "sr","srr"]
  print $ [d3_dot "r"    y | y <- d3_all] == [  "r", "rr",  "e","srr",  "s", "sr"]
  print $ [d3_dot "rr"   y | y <- d3_all] == [ "rr",  "e",  "r", "sr","srr",  "s"]
  print $ [d3_dot "s"    y | y <- d3_all] == [  "s", "sr","srr",  "e",  "r", "rr"]
  print $ [d3_dot "sr"   y | y <- d3_all] == [ "sr","srr",  "s", "rr",  "e",  "r"]
  print $ [d3_dot "srr"  y | y <- d3_all] == ["srr",  "s", "sr",  "r", "rr",  "e"]
  putStrLn $ "s * r * s = " ++ head (d3 ["s", "r", "s"])
  putStrLn $ "(sr)^(-1) = " ++ d3_inv "sr"

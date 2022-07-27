k4_sym :: String -> (Int, Int)
k4_sym "e" = (0, 0)
k4_sym "p" = (0, 1)
k4_sym "q" = (1, 0)
k4_sym "r" = (1, 1)
k4_sym  _  = error "ERROR: Invalid argument."

showTuple :: (Int, Int) -> String
showTuple (0, 0) = "e"
showTuple (0, 1) = "p"
showTuple (1, 0) = "q"
showTuple (1, 1) = "r"
showTuple _ = error "ERROR: Invalid argument."

c2_op :: Int -> Int -> Int
c2_op x y = (x + y) `mod` 2

k4_op :: (Int, Int) -> (Int, Int) -> (Int, Int)
k4_op (x1, y1) (x2, y2) = (c2_op x1 x2, c2_op y1 y2)

k4_inv :: (Int, Int) -> (Int, Int)
k4_inv (x, y) = head [(p, q) | p <- [0..2], q <- [0..2], k4_op (x, y) (p, q) == k4_sym "e"]

k4_eval :: [(Int, Int)] -> [(Int, Int)]
k4_eval [] = []
k4_eval [x] = [x]
k4_eval [x, y] = (k4_op x y):[]
k4_eval (x:y:zs) = k4_eval ((k4_eval [x, y]) ++ zs)

main :: IO ()
main = do
  putStrLn $ "p^(-1) = " ++ showTuple (k4_inv (k4_sym "p"))
  putStrLn $ "r^(-1) = " ++ showTuple (k4_inv (k4_sym "r"))
  putStrLn $ "p q = " ++ showTuple (head (k4_eval [k4_sym "p", k4_sym "q"]))
  putStrLn $ "p q r = " ++ showTuple (head (k4_eval [k4_sym "p", k4_sym "q", k4_sym "r"]))

data K4 = C2xC2 Int Int

instance Eq K4 where
  (C2xC2 x y) == (C2xC2 x' y') = x == x' && y == y'

instance Show K4 where
  show a = case a of
    C2xC2 0 0 -> "e"
    C2xC2 0 1 -> "p"
    C2xC2 1 0 -> "q"
    C2xC2 1 1 -> "r"
    _         -> "?"

k4_sym :: String -> K4
k4_sym "e" = C2xC2 0 0
k4_sym "p" = C2xC2 0 1
k4_sym "q" = C2xC2 1 0
k4_sym "r" = C2xC2 1 1
k4_sym  _  = error "ERROR: Invalid argument."

c2_op :: Int -> Int -> Int
c2_op x y = (x + y) `mod` 2

k4_op :: K4 -> K4 -> K4
k4_op (C2xC2 x1 y1) (C2xC2 x2 y2) = C2xC2 (c2_op x1 x2) (c2_op y1 y2)

k4_inv :: K4 -> K4
k4_inv (C2xC2 x y) = head [C2xC2 p q | p <- [0..2], q <- [0..2], k4_op (C2xC2 x y) (C2xC2 p q) == k4_sym "e"]

k4_eval :: [K4] -> [K4]
k4_eval [] = []
k4_eval [x] = [x]
k4_eval [x, y] = (k4_op x y):[]
k4_eval (x:y:zs) = k4_eval ((k4_eval [x, y]) ++ zs)

main :: IO ()
main = do
  putStrLn $ "p^(-1) = " ++ show (k4_inv (k4_sym "p"))
  putStrLn $ "r^(-1) = " ++ show (k4_inv (k4_sym "r"))
  putStrLn $ "p q = " ++ show (head (k4_eval [k4_sym "p", k4_sym "q"]))
  putStrLn $ "p q r = " ++ show (head (k4_eval [k4_sym "p", k4_sym "q", k4_sym "r"]))

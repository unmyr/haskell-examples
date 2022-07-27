k4_e :: (Int, Int)
k4_e = (0, 0)

k4_p :: (Int, Int)
k4_p = (0, 1)

k4_q :: (Int, Int)
k4_q = (1, 0)

k4_r :: (Int, Int)
k4_r = (1, 1)

toSymbol :: (Int, Int) -> String
toSymbol (0, 0) = "e"
toSymbol (0, 1) = "p"
toSymbol (1, 0) = "q"
toSymbol (1, 1) = "r"
toSymbol _ = error "ERROR: Invalid argument."

k4_op :: (Int, Int) -> (Int, Int) -> (Int, Int)
k4_op (x1, y1) (x2, y2) = ((x1 + x2) `mod` 2, (y1 + y2) `mod` 2)

k4_inv :: (Int, Int) -> (Int, Int)
k4_inv (x, y) = head [(p, q) | p <- [0..2], q <- [0..2], k4_op (x, y) (p, q) == k4_e]

k4 :: [(Int, Int)] -> [(Int, Int)]
k4 [] = []
k4 [x] = [x]
k4 (x:xs) = k4 ([k4_op x (head xs)] ++ (tail xs))

main :: IO ()
main = do
  putStrLn $ "p^(-1) = " ++ toSymbol (k4_inv k4_p)
  putStrLn $ "r^(-1) = " ++ toSymbol (k4_inv k4_r)
  putStrLn $ "p q = " ++ toSymbol (head (k4 [k4_p, k4_q]))
  putStrLn $ "p q r = " ++ toSymbol (head (k4 [k4_p, k4_q, k4_r]))

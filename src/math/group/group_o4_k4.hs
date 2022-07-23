e = (0, 0)
p = (0, 1)
q = (1, 0)
r = (1, 1)

toSymbol (0, 0) = "e"
toSymbol (0, 1) = "p"
toSymbol (1, 0) = "q"
toSymbol (1, 1) = "r"

k4_op (x1, y1) (x2, y2) = ((x1 + x2) `mod` 2, (y1 + y2) `mod` 2)
k4_inv (x, y) = head [(p, q) | p <- [0..2], q <- [0..2], k4_op (x, y) (p, q) == (0, 0)]

k4 :: [(Int, Int)] -> [(Int, Int)]
k4 [] = []
k4 [x] = [x]
k4 (x:xs) = k4 ([k4_op x (head xs)] ++ (tail xs))

main = do
  putStrLn $ "p^(-1) = " ++ toSymbol (k4_inv p)
  putStrLn $ "r^(-1) = " ++ toSymbol (k4_inv r)
  putStrLn $ "p q = " ++ toSymbol (head (k4 [p, q]))
  putStrLn $ "p q r = " ++ toSymbol (head (k4 [p, q, r]))

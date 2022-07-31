-- An example of Semidirect products
data D3 = C3xdC2 Int Int

instance Eq D3 where
  (C3xdC2 x y) == (C3xdC2 x' y') = x == x' && y == y'

d3_sym :: String -> D3
d3_sym "e"   = C3xdC2 0 0
d3_sym "r"   = C3xdC2 1 0
d3_sym "rr"  = C3xdC2 2 0
d3_sym "s"   = C3xdC2 0 1
d3_sym "sr"  = C3xdC2 2 1
d3_sym "srr" = C3xdC2 1 1
d3_sym  _  = error "ERROR: Invalid argument."

d3_all :: [D3]
d3_all = [d3_sym "e", d3_sym "r", d3_sym "rr", d3_sym "s", d3_sym "sr", d3_sym "srr"]

instance Show D3 where
  show a = case a of
    C3xdC2 0 0 -> "e"
    C3xdC2 1 0 -> "r"
    C3xdC2 2 0 -> "rr"
    C3xdC2 0 1 -> "s"
    C3xdC2 1 1 -> "srr"
    C3xdC2 2 1 -> "sr"
    C3xdC2 r s -> "(" ++ show r ++ "," ++ show s ++ ")"

phi :: Int -> Int -> Int
phi 0 _  = 0
phi 1 r' = r'
phi _ _  = error "ERROR: Invalid arguments."

instance Semigroup D3 where
  -- semidirect products: (r_x, s_x) <> (r_y, s_y) = (r_x * r_y * phi(s_x)(r_y), s_x * s_y)
  (C3xdC2 r s) <> (C3xdC2 r' s') = C3xdC2 ((r + r' + (phi s r')) `mod` 3) ((s + s') `mod` 2)

instance Monoid D3 where
  mempty = C3xdC2 0 0

d3_inv :: D3 -> D3
d3_inv (C3xdC2 r_x s_x) = head [C3xdC2 r_y s_y | r_y <- [0, 1, 2], s_y <- [0, 1], (C3xdC2 r_x s_x) <> (C3xdC2 r_y s_y) == (mempty)]

main :: IO ()
main = do
  print $ [(d3_sym "e")   <> y | y <- d3_all] == [  d3_sym "e",   d3_sym "r",  d3_sym "rr",   d3_sym "s",  d3_sym "sr", d3_sym "srr"]
  print $ [(d3_sym "r")   <> y | y <- d3_all] == [  d3_sym "r",  d3_sym "rr",   d3_sym "e", d3_sym "srr",   d3_sym "s",  d3_sym "sr"]
  print $ [(d3_sym "rr")  <> y | y <- d3_all] == [ d3_sym "rr",   d3_sym "e",   d3_sym "r",  d3_sym "sr", d3_sym "srr",   d3_sym "s"]
  print $ [(d3_sym "s")   <> y | y <- d3_all] == [  d3_sym "s",  d3_sym "sr", d3_sym "srr",   d3_sym "e",   d3_sym "r",  d3_sym "rr"]
  print $ [(d3_sym "sr")  <> y | y <- d3_all] == [ d3_sym "sr", d3_sym "srr",   d3_sym "s",  d3_sym "rr",   d3_sym "e",   d3_sym "r"]
  print $ [(d3_sym "srr") <> y | y <- d3_all] == [d3_sym "srr",   d3_sym "s",  d3_sym "sr",   d3_sym "r",  d3_sym "rr",   d3_sym "e"]
  print $ mconcat [d3_sym "s", d3_sym "r", d3_sym "s"] == d3_sym "rr"
  putStrLn $ "s * r * s = " ++ show (mconcat [d3_sym "s", d3_sym "r", d3_sym "s"]) -- == d3_sym "rr"
  print $ d3_inv (d3_sym "sr") == d3_sym "sr"
  putStrLn $ "(sr)^(-1) = " ++ show (d3_inv (d3_sym "sr")) -- == d3_sym "sr"

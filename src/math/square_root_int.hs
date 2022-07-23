-- squareRoot :: Int -> Int
squareRoot :: (Eq a, Ord a, Num a, Integral a) => a -> a
squareRoot n | n == 0 = 0
squareRoot n | n == 1 = 1
squareRoot n | n < 0  = error "ERROR: number must be positive."
squareRoot n = squareRootHelper n (n `div` 2 + 1)
  where 
  squareRootHelper n_ k
    | 1 < k && n_ == k*k = k
    | 1 < k && n_  < k*k = squareRootHelper n_ (k-1)
    | otherwise          = error "ERROR: Square root not found."

-- squareRootM :: Int -> Maybe Int
squareRootM :: (Eq a, Ord a, Num a, Integral a) => a -> Maybe a
squareRootM n | n<0 = Nothing
squareRootM n = do
  let match_list = [ x | x <- [2..(n `div` 2) + 1], x^(2 :: Int) == n]
  case length match_list == 0 of
    True -> Nothing
    False -> Just $ head $ match_list

main :: IO ()
main = do
  print $ squareRoot (25 :: Int)
  print $ squareRootM (25 :: Int)

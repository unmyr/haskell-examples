squareRoot :: Int -> Int
squareRoot n | n == 0 = 0
squareRoot n | n == 1 = 1
squareRoot n | n >= 2 = squareRootHelper n (n `div` 2 + 1)
  where 
  squareRootHelper n k
    | 1 < k && n == k*k = k
    | 1 < k && n < k*k = squareRootHelper n (k-1)

squareRootM :: Int -> Maybe Int
squareRootM n | n<0 = Nothing
squareRootM n | n>=0 = do
  let match_list = [ x | x <- [2..(n `div` 2) + 1], x^2 == n]
  case length match_list == 0 of
    True -> Nothing
    False -> Just $ head $ match_list

digitSum :: Int -> Int
digitSum = digitSumHelper 0
  where
    digitSumHelper ds n =
      if n >= 10 then
        digitSumHelper (ds + (n `mod` 10)) (n `div` 10)
      else
        ds + n

hasNthRoot :: Int -> Int -> Bool
hasNthRoot n 1 = n == 1
hasNthRoot n p = hasNthRootHelper n p
  where
    hasNthRootHelper n p
      | p < n && n `mod` p == 0 = hasNthRootHelper (n `div` p) p
      | n <= p && n `mod` p == 0 = True
      | n `mod` p /= 0 = False

getNthRoot :: Int -> Int -> (Bool,Int,Int,Int)
getNthRoot n 1 = (n == 1, n, 1, 1)
getNthRoot n p = getNthRootHelper n n p 1
  where
    getNthRootHelper n r p exponent
      | p < r && r `mod` p == 0 = getNthRootHelper n (r `div` p) p (exponent + 1)
      | r <= p && r `mod` p == 0 = (True, n, p, exponent)
      | r `mod` p /= 0 = (False, n, p, exponent)

showDetails :: (Bool,Int,Int,Int) -> String
showDetails (_, number, base, exponent) =
    "" ++ show number ++ " (" ++ show base ++ "^" ++ show exponent ++ ")"

getMatch :: (Bool,Int,Int,Int) -> Bool
getMatch (match, _, _, _) = match

main :: IO ()
main = do
  let range = [1000..9999]
  -- let number_list = [ n | n <- range, hasNthRoot n (digitSum n)]
  -- print(number_list)
  -- mapM_ putStrLn [showDetails (getNthRoot n (digitSum n)) | n <- number_list, hasNthRoot n (digitSum n)]
  mapM_ putStrLn [showDetails (getNthRoot n (digitSum n)) | n <- range, getMatch (getNthRoot n (digitSum n))]

hasCubeRoot :: Int -> Bool
hasCubeRoot n | n == 0 = True
hasCubeRoot n | n == 1 = True
hasCubeRoot n | n >= 2 = cubeRootHelper n (n `div` 2 + 1)
  where 
    cubeRootHelper n k
      | 1 < k && n == k*k*k = True
      | 1 < k && n < k*k*k = cubeRootHelper n (k-1)
      | otherwise = False
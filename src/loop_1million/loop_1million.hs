import Data.Time.Clock

main = do
    let sum_1million total
          | total < 10^6 = do
              sum_1million (total + 1)
          | otherwise = return total
    t0 <- getCurrentTime
    sum_1million 0 >>= print
    t1 <- getCurrentTime
    print (t1 `diffUTCTime` t0)

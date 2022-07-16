import Criterion.Main

sum'(x:xs) = x + sum(xs)
sum_foldr(a) = foldr (+) 0 a
sum_foldl(a) = foldl (+) 0 a

main = do
  defaultMain [
    bgroup "sum" [
      bench "sum'" $ whnf sum' [1..1000000],
      bench "sum(foldr)" $ whnf sum' [1..1000000],
      bench "sum(foldl)" $ whnf sum' [1..1000000]
      ]
    ]


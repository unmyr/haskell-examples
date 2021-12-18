compute :: (Double -> Double -> Double) -> Double
compute f = f 3 4 ::Double

main = do
    let hypot = \x y -> sqrt (x^2 + y^2)
    print $ hypot 5 12
    print $ compute hypot
    print $ compute (**)
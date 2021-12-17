compute :: ((Double, Double) -> Double) -> Double
compute (f) = f (3::Double, 4::Double)

hypot :: (Double, Double) -> Double
hypot (x, y) = sqrt (x^2 + y^2)

main = do
    print $ hypot(5, 12)
    print $ compute(hypot)
    print $ compute((\(x, y) -> x**y))
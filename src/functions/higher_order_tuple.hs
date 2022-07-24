compute :: ((Double, Double) -> Double) -> Double
compute (f) = f (3::Double, 4::Double)

main :: IO ()
main = do
    let hypot_f = \(x, y) -> sqrt (x ** 2 + y ** 2)
    print $ hypot_f(5, 12)
    print $ compute(hypot_f)
    print $ compute((\(x, y) -> x**y))

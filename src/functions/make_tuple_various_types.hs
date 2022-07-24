make_tuple :: (Integral a, Fractional b, Floating c) => a -> b -> c -> String -> (a, b, c, String)
make_tuple a b c d = (a, b, c, d)

main :: IO ()
main = do
    print $ make_tuple (3 :: Int) (1/3 :: Rational) (3.14 :: Float) "hello"
    print $ make_tuple (3 :: Integer) (1/3 :: Rational) (3.14 :: Double) "world"

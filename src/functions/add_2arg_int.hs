add :: Int -> Int -> Int
add x y = x + y

main :: IO ()
main = do
    print(add 42 13)
    print $ add (maxBound :: Int) 0 == 9223372036854775807
    print $ add (maxBound :: Int) 1 == (minBound :: Int)
    print $ add (minBound :: Int) (-1) == (maxBound :: Int)

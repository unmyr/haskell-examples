add :: (Integer, Integer) -> Integer
add (x, y) = x + y

main :: IO ()
main = do
    print(add(42, 13))

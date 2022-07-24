add :: (Num a) => a -> a -> a
add x y = x + y

main :: IO ()
main = do
    print $ add (42 :: Int) 13      == 55
    print $ add (42 :: Integer) 13  == 55
    print $ add (42 :: Rational) 13 == 55
    print $ add (42 :: Float) 13    == 55
    print $ add (42 :: Double) 13   == 55
    -- NG: Int type overflow 
    print $
      add (maxBound :: Int) 1             == (minBound :: Int)
    -- OK: Integer type
    print $
      add (toInteger (maxBound :: Int)) 1 == (toInteger (maxBound :: Int)) + 1
    -- NG: Int type underflow
    print $
      add (minBound :: Int) (-1)             == (maxBound :: Int)
    -- OK: Integer type
    print $
      add (toInteger (minBound :: Int)) (-1) == (toInteger (minBound :: Int)) -1

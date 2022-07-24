add_int_and_float :: Integer -> Float -> Float
add_int_and_float x y = fromIntegral(x) + y

main :: IO ()
main = do
    print(add_int_and_float 1 1.5)
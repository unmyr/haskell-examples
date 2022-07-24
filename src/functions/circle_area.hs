circle_area_float :: Float -> Float
circle_area_float r = pi * r * r

circle_area_double :: Double -> Double
circle_area_double r = pi * r * r

circle_area_floating :: Floating a => a -> a
circle_area_floating r = pi * r * r

main :: IO ()
main = do
    print $ circle_area_float 1
    print $ circle_area_double 1
    print $ circle_area_floating (1 :: Float)
    print $ circle_area_floating (1 :: Double)

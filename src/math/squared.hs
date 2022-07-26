import Data.Complex

sqInt :: Int -> Int
sqInt x = x * x

sqInteger :: Integer -> Integer
sqInteger x = x * x

sqIntegral :: Integral a => a -> a
sqIntegral x = x * x

sqFloat :: Float -> Float
sqFloat x = x * x

sqDouble :: Double -> Double
sqDouble x = x * x

sqFloating :: Floating a => a -> a
sqFloating x = x * x

sqComplex :: (RealFloat a) => Complex a -> Complex a
sqComplex x = x * x

sqFractional :: Fractional a => a -> a
sqFractional x = x * x

sqReal :: Real a => a -> a
sqReal x = x * x

sqRealFrac :: RealFrac a => a -> a
sqRealFrac x = x * x

sqRealFloat :: RealFloat a => a -> a
sqRealFloat x = x * x

sqNum:: Num a => a -> a
sqNum x = x * x

main :: IO ()
main = do
  print $ sqInt 3037000499 == 9223372030926249001
  print $ sqInteger 3037000499 == 9223372030926249001
  print $ sqIntegral (3037000499 :: Int) == 9223372030926249001
  print $ sqInt 3037000500 == (-9223372036709301616) -- underflow
  print $ sqIntegral (3037000500 :: Int) == (-9223372036709301616) -- underflow
  print $ sqInteger 3037000500               == 9223372037000250000
  print $ sqIntegral (3037000500 :: Integer) == 9223372037000250000
  print $ sqFloat (sqrt(2)::Float) == 1.9999999
  print $ sqFloating (sqrt(2)::Float) == 1.9999999
  print $ sqFractional (sqrt(2)::Float) == 1.9999999
  print $ sqReal (sqrt(2)::Float) == 1.9999999
  print $ sqRealFrac (sqrt(2)::Float) == 1.9999999
  print $ sqRealFloat (sqrt(2)::Float) == 1.9999999
  print $ sqNum (sqrt(2)::Float) == 1.9999999
  print $ sqComplex ((sqrt(2)::Float) :+ 0) == 1.9999999 :+ 0.0
  print $ sqDouble (sqrt(2)::Double) == 2.0000000000000004
  print $ sqFloating (sqrt(2)::Double) == 2.0000000000000004
  print $ sqFractional (sqrt(2)::Double) == 2.0000000000000004
  print $ sqRealFrac (sqrt(2)::Double) == 2.0000000000000004
  print $ sqReal (sqrt(2)::Double) == 2.0000000000000004
  print $ sqRealFloat (sqrt(2)::Double) == 2.0000000000000004
  print $ sqNum (sqrt(2)::Double) == 2.0000000000000004
  print $ sqFractional (1/3 :: Rational) == 1/9
  print $ sqRealFrac (1/3 :: Rational) == 1/9
  print $ sqReal (1/3 :: Rational) == 1/9
  print $ sqNum (1/3 :: Rational) == 1/9
  print $ sqComplex ((0::Float) :+ (1::Float)) == -1
  print $ sqFractional ((0::Float) :+ (1::Float)) == -1
  print $ sqNum ((0::Float) :+ (1::Float)) == -1
  let omega_f = ((-0.5)::Float) :+ 0.5 * sqrt(3)
  print $ omega_f * sqComplex omega_f == 1
  let omega_d = ((-0.5)::Double) :+ 0.5 * sqrt(3)
  print $ omega_d * sqComplex omega_d == 0.9999999999999998 :+ 1.1102230246251565e-16

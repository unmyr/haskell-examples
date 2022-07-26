import Data.Complex

cubed :: (RealFloat a) => Complex a -> Complex a
cubed x = x * x * x

main :: IO ()
main = do
  let omega_f = ((-0.5)::Float) :+ 0.5 * sqrt(3)
  print $ cubed omega_f == 1
  print $ omega_f ** 3 == 1.0 :+ 1.7484555e-7
  let omega_d = ((-0.5)::Double) :+ 0.5 * sqrt(3)
  print $ cubed omega_d == 0.9999999999999998 :+ 1.1102230246251565e-16

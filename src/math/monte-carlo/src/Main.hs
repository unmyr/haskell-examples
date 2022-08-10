module Main where
import System.Random
import System.Environment (getArgs)
import Text.Printf

computeNorms :: [Double] -> [Double]
computeNorms []  = []
computeNorms [x] = []
computeNorms (x:y:xs2) = (x*x + y*y) : (computeNorms xs2)

calcPi :: Int -> [Double] -> Double
calcPi n xs =
  (\x -> ((4 :: Double) * (fromIntegral x))/(fromIntegral n))
  $ length $ filter (== True) $ take n
  $ map (\x -> x < 1) xs

main :: IO ()
main = do
  args <- getArgs
  let str_num_trials = args !! 0
      num_trials = read str_num_trials :: Int

  -- gen <- newStdGen
  -- let monte_carlo_pi = calcPi num_trials (computeNorms (randomRs (0, 1) (gen) :: [Double]))

  let seed = 1
  let monte_carlo_pi = calcPi num_trials (computeNorms (randomRs (0, 1) (mkStdGen seed) :: [Double]))

  printf "trial=%d, result=%f, error=%.10f\n" (num_trials) (monte_carlo_pi) (abs (monte_carlo_pi - pi))

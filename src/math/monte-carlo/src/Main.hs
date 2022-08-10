module Main where
import System.Random
import System.Environment (getArgs)
import Text.Printf

computeNorms :: [Double] -> [Double]
computeNorms []  = []
computeNorms [_] = []
computeNorms (x:y:xs2) = (x*x + y*y) : (computeNorms xs2)

calcPi :: Int -> [Double] -> Double
calcPi n xs =
  (\x -> ((4 :: Double) * (fromIntegral x))/(fromIntegral n))
  $ length $ filter (== True) $ take n
  $ map (\x -> x < 1) xs

runMonteCarloNoSeed :: Int -> IO ()
runMonteCarloNoSeed num_trials = do
  gen <- newStdGen
  let monte_carlo_pi = calcPi num_trials (computeNorms (randomRs (0, 1) (gen) :: [Double]))
  printf "trial=%10d, result=%.10f, error=%.10f\n" (num_trials) (monte_carlo_pi) (abs (monte_carlo_pi - pi))

runMonteCarloWithSeed :: Int -> Int -> IO ()
runMonteCarloWithSeed num_trials seed = do
  let monte_carlo_pi = calcPi num_trials (computeNorms (randomRs (0, 1) (mkStdGen seed) :: [Double]))
  printf "trial=%10d, result=%.10f, error=%.10f\n" (num_trials) (monte_carlo_pi) (abs (monte_carlo_pi - pi))

main :: IO ()
main = do
  args <- getArgs
  if 1 == length args then do
    let str_num_trials = args !! 0
        num_trials = read str_num_trials :: Int
    runMonteCarloWithSeed num_trials 1
  else do
    -- [1, 10, 100, 1000, 10000, 100000, 1000000, 10000000]
    let trialList = map (\n -> (10::Int)^n) ([0..7] :: [Int])

    putStrLn "-- runMonteCarloNoSeed"
    mapM_ (\n -> runMonteCarloNoSeed n) trialList

    putStrLn $ "\n-- runMonteCarloWithSeed: seed=" ++ (show seed)
    let seed = 1
    mapM_ (\n -> runMonteCarloWithSeed n seed) trialList

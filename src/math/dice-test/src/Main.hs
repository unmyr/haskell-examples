module Main where
import System.Random

main :: IO ()
main = getStdRandom (randomR (1::Int, 6)) >>= print

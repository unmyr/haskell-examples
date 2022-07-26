import Control.Monad.State

a :: State s Int
a = pure 1

main = do
  print $ runState a ()

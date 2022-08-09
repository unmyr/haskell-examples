import qualified Data.Char (chr, ord)

main :: IO ()
main = do
  -- OK
  print $ (Data.Char.chr . Data.Char.ord) 'a'

  -- NG: Variable not in scope: chr :: b0 -> a0
  -- print $ (chr . ord) 'a'
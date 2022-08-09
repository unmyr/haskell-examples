import qualified Data.Char as C (chr, ord)

main :: IO ()
main = do
  -- NG: No module named ‘Data.Char’ is imported.
  -- print $ (Data.Char.chr . Data.Char.ord) 'a'

  -- OK
  print $ (C.chr . C.ord) 'a'

  -- NG: Variable not in scope: chr :: b0 -> a0
  -- print $ (chr . ord) 'a'

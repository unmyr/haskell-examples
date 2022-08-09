import Data.Char as C (chr, ord)

main :: IO ()
main = do
  -- print $ (Data.Char.chr . Data.Char.ord) 'a' -- NG
  print $ (C.chr . C.ord) 'a'
  print $ (chr . ord) 'a'

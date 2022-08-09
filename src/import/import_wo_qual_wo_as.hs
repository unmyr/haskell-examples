import Data.Char (chr, ord)

main :: IO ()
main = do
  print $ (Data.Char.chr . Data.Char.ord) 'a'
  print $ (chr . ord) 'a'

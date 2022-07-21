import Data.List

data Fruits = Apple | Orange | Kiwi deriving (Enum, Show)

talk :: Fruits -> String 
talk sym = what sym
  where what Apple = "I like apples."
        what Orange = "I like oranges."
        what Kiwi = "I like kiwis."

main = do
  let fruits = [Apple, Orange, Kiwi]
  putStrLn
    $ Data.List.intercalate "\n"
    $ map (\sym -> show sym ++ ": " ++ talk sym) fruits

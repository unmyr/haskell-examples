import Data.List

data Fruits = Apple | Orange | Kiwi deriving (Enum, Show)

talk :: Fruits -> String 
talk Apple = "I like apples."
talk Orange = "I like oranges."
talk Kiwi = "I like kiwis."

main = do
  let fruits = [Apple, Orange, Kiwi]
  putStrLn
    $ Data.List.intercalate "\n"
    $ map (\sym -> show sym ++ ": " ++ talk sym) fruits

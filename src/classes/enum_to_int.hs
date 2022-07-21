data Fruits = Apple | Orange | Kiwi deriving (Enum)

main = do
    print $ fromEnum Apple
    print $ fromEnum Orange
    print $ fromEnum Kiwi
    print $ map fromEnum [Apple, Orange, Kiwi]

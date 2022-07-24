get_int_list :: Int -> [Int]
get_int_list n = [n] ++ [n]

main :: IO ()
main = do
    print(get_int_list 3)

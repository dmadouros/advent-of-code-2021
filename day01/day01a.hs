countDepths :: (Ord a, Num a) => [a] -> a
countDepths [] = 0
countDepths (x:[]) = 0
countDepths (x:y:xs) = count x y + countDepths (y:xs)
    where count a b = case a >= b of True -> 0
                                     False -> 1

stringToInt :: String -> Int
stringToInt = read

main = do
    contents <- getContents
    let depths = map stringToInt . lines $ contents
    let total = countDepths depths
    putStr (show total)

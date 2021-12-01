countDepths :: (Ord a, Num a) => [a] -> a
countDepths [] = 0
countDepths (_:[]) = 0
countDepths (_:_:[]) = 0
countDepths (_:_:_:[]) = 0
countDepths (w:x:y:z:xs) = count sumA sumB + countDepths (x:y:z:xs)
    where sumA = sum [w, x, y]
          sumB = sum [x, y, z]
          count a b = case a >= b of True -> 0
                                     False -> 1

toInt :: String -> Int
toInt = read

main = do
    contents <- getContents
    let depths = map (toInt) (lines contents)
    let total = countDepths depths
    putStr (show total)

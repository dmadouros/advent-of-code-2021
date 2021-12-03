import Data.Char

mostCommonBit :: String -> Char
mostCommonBit xs = if count1 >= count0 then '1' else '0'
    where count1 = length . filter (== '1') $ xs
          count0 = length . filter (== '0') $ xs

leastCommonBit :: String -> Char
leastCommonBit xs = if count1 < count0 then '1' else '0'
    where count1 = length . filter (== '1') $ xs
          count0 = length . filter (== '0') $ xs

binaryToInt :: String -> Int
binaryToInt = foldl (\acc x -> acc * 2 + digitToInt x) 0

extractBits :: ([String], [String]) -> [String]
extractBits (hs, []) = hs
extractBits (hs, ts) = extractBits (hs ++ [heads], tails)
    where heads = map head ts
          tails = filter (/= "") . map tail $ ts

calculateRating :: (String -> Char) -> Int -> [String] -> String
calculateRating f _ [x] = x
calculateRating f idx xs = calculateRating f (idx + 1) remainder
    where targets = map f $ extractBits ([], xs)
          target = targets !! idx
          remainder = filter ((== target) . (!! idx)) $ xs

calculateO2Rating :: [String] -> String
calculateO2Rating = calculateRating mostCommonBit 0

calculateCo2Rating :: [String] -> String
calculateCo2Rating = calculateRating leastCommonBit 0

main = do
    contents <- getContents
    let diagnostics = lines $ contents
    let o2GeneratorRating = binaryToInt . calculateO2Rating $ diagnostics
    let co2ScrubberRating = binaryToInt . calculateCo2Rating $ diagnostics
    let result = co2ScrubberRating * o2GeneratorRating

    putStr (show result)
import Data.Char

mostCommonBit :: String -> Char
mostCommonBit xs = if count1 > count0 then '1' else '0'
    where count1 = length . filter (== '1') $ xs
          count0 = length . filter (== '0') $ xs

binaryToInt :: String -> Int
binaryToInt = foldl (\acc x -> acc * 2 + digitToInt x) 0

extractBits :: ([String], [String]) -> [String]
extractBits (hs, []) = hs
extractBits (hs, ts) = extractBits (hs ++ [heads], tails)
    where heads = map head ts
          tails = filter (/= "") . map tail $ ts

compliment :: String -> String
compliment = map (\x -> if x == '1' then '0' else '1')

main = do
    contents <- getContents
    let diagnostics = lines $ contents
    let gammaBin = map mostCommonBit $ extractBits ([], diagnostics)
    let epsilonBin = compliment $ gammaBin
    let gamma = binaryToInt $ gammaBin
    let epsilon = binaryToInt $ epsilonBin
    let result = gamma * epsilon

    putStr (show result)
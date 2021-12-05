import Data.Char
import Data.List

stringToInt :: String -> Int
stringToInt = read

skipChar :: String -> String
skipChar record = dropWhile (not . isDigit) record

getDigits :: String -> String
getDigits record = takeWhile (isDigit) record

parseLine :: [String] -> String -> ((Int, Int), (Int, Int))
parseLine [x1, y1, x2, y2] [] = (((stringToInt x1), (stringToInt y1)), ((stringToInt x2), (stringToInt y2)))
parseLine result record = parseLine (result ++ [digits]) record'
    where record' = skipChar . dropWhile (isDigit) $ record
          digits = getDigits record

points vector@((x1, y1), (x2, y2))
    | x1 == x2  = pointsVertical vector
    | y1 == y2  = pointsHorizontal vector
    | otherwise = pointsDiagonal vector

pointsHorizontal ((x1, y1), (x2, y2)) = zip [minX..maxX] (repeat y1)
    where maxX = max x1 x2
          minX = min x1 x2

pointsVertical ((x1, y1), (x2, y2)) = zip (repeat x1) [minY..maxY]
    where maxY = max y1 y2
          minY = min y1 y2

pointsDiagonal ((x1, y1), (x2, y2)) = zip xPoints yPoints
    where maxX = max x1 x2
          maxY = max y1 y2 
          minX = min x1 x2
          minY = min y1 y2
          xPoints = if x1 < x2 then [minX..maxX] else (reverse [minX..maxX])
          yPoints = if y1 < y2 then [minY..maxY] else (reverse [minY..maxY])

main = do
  contents <- getContents
  let records = lines $ contents
  let vectors = map (parseLine []) $ records
  let result = length . filter (\xs -> length xs > 1) . group . sort . concat . map points $ vectors
  putStr (show result)

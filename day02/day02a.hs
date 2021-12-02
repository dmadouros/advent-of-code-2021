toPair :: [a] -> (a, a)
toPair [x, y] = (x, y)

toInt :: String -> Int
toInt = read

distanceToInt :: (String, String) -> (String, Int)
distanceToInt (direction, distance) = (direction, toInt distance)

calcDistances :: (Int, Int) -> (String, Int) -> (Int, Int)
calcDistances (h, d) (dir, dist) =
    case dir of "forward" -> (h + dist, d)
                "up"      -> (h, d - dist)
                "down"    -> (h, d + dist)

main = do
    contents <- getContents
    let instructions = map (distanceToInt) . map (toPair) . map (words) . lines $ contents
    let (horizontal, depth) = foldl (calcDistances) (0, 0) instructions
    let result = horizontal * depth
    putStr (show result)

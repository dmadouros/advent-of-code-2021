toPair :: [a] -> (a, a)
toPair [x, y] = (x, y)

stringToInt :: String -> Int
stringToInt = read

distanceToInt :: (String, String) -> (String, Int)
distanceToInt (direction, distance) = (direction, stringToInt distance)

calcDistance :: (Int, Int) -> (String, Int) -> (Int, Int)
calcDistance (h, d) (dir, dist) =
    case dir of "forward" -> (h + dist, d)
                "up"      -> (h, d - dist)
                "down"    -> (h, d + dist)

main = do
    contents <- getContents
    let instructions = map distanceToInt . map toPair . map words . lines $ contents
    let (horizontal, depth) = foldl calcDistance (0, 0) instructions
    let result = horizontal * depth
    putStr (show result)

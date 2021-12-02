toPair :: [a] -> (a, a)
toPair [x, y] = (x, y)

stringToInt :: String -> Int
stringToInt = read

distanceToInt :: (String, String) -> (String, Int)
distanceToInt (direction, distance) = (direction, stringToInt distance)

calcDistance :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
calcDistance (a, h, d) (dir, dist) =
    case dir of "forward" -> (a, h + dist, d + (a * dist))
                "up"      -> (a - dist, h, d)
                "down"    -> (a + dist, h, d)

main = do
    contents <- getContents
    let instructions = map distanceToInt . map toPair . map words . lines $ contents
    let (aim, horizontal, depth) = foldl calcDistance (0, 0, 0) instructions
    let result = horizontal * depth
    putStr (show result)

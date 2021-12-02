toPair :: [a] -> (a, a)
toPair [x, y] = (x, y)

stringToInt :: String -> Int
stringToInt = read

distanceToInt :: (String, String) -> (String, Int)
distanceToInt (direction, distance) = (direction, stringToInt distance)

move :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
move (aim, horizontal, depth) (direction, distance) =
    case direction of "forward" -> (aim, horizontal + distance, depth + (aim * distance))
                      "up"      -> (aim - distance, horizontal, depth)
                      "down"    -> (aim + distance, horizontal, depth)

main = do
    contents <- getContents
    let instructions = map distanceToInt . map toPair . map words . lines $ contents
    let (_, horizontal, depth) = foldl move (0, 0, 0) $ instructions
    let result = horizontal * depth
    putStr (show result)

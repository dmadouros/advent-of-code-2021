toPair :: [a] -> (a, a)
toPair [x, y] = (x, y)

stringToInt :: String -> Int
stringToInt = read

distanceToInt :: (String, String) -> (String, Int)
distanceToInt (direction, distance) = (direction, stringToInt distance)

move :: (Int, Int) -> (String, Int) -> (Int, Int)
move (horizontal, depth) (direction, distance) =
    case direction of "forward" -> (horizontal + distance, depth)
                      "up"      -> (horizontal, depth - distance)
                      "down"    -> (horizontal, depth + distance)

main = do
    contents <- getContents
    let instructions = map distanceToInt . map toPair . map words . lines $ contents
    let (horizontal, depth) = foldl move (0, 0) $ instructions
    let result = horizontal * depth
    putStr (show result)

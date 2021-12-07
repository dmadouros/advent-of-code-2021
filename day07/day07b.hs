import Data.Text.Lazy(pack)


main = do
  contents <- getContents
  let actualPositions = read $ "[" ++ contents ++ "]" :: [Integer]

  let maxPosition = maximum actualPositions  
  let minPosition = minimum actualPositions

  let targetPositions = [minPosition..maxPosition]

  let fuelBurned = minimum . map (\y -> sum . map (\x -> sum [1..(abs (y - x))]) $ actualPositions) $ targetPositions

  print fuelBurned

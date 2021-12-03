import Data.Char
import Data.List
import Data.Function

type Binary = [Char]
type Bit = Char
type Bits = [Bit]

commonBitWith :: 
    ((Bits -> Bits -> Ordering) -> [Bits] -> Bits)
    -> (Bits -> Int) 
    -> Bits 
    -> Bit
commonBitWith f g = head . f (compare `on` g) . pairToList . partition (== '0')
    where pairToList = (\(x, y) -> [x, y])

mostCommonBitWith :: Bits -> Bit
mostCommonBitWith = commonBitWith maximumBy length

leastCommonBitWith :: Bits -> Bit
leastCommonBitWith = commonBitWith minimumBy length

binaryToInt :: Binary -> Int
binaryToInt = foldl (\acc x -> acc * 2 + digitToInt x) 0

calculateRatingBy :: (Bits -> Bit) -> Int -> [Binary] -> Int
calculateRatingBy f _ [x] = binaryToInt x
calculateRatingBy f idx xs = calculateRatingBy f (idx + 1) remainder
    where extractBits = transpose
          targets = map f $ extractBits xs
          target = targets !! idx
          remainder = filter ((== target) . (!! idx)) $ xs

calculateO2Rating :: [Binary] -> Int
calculateO2Rating = calculateRatingBy mostCommonBitWith 0

calculateCo2Rating :: [Binary] -> Int
calculateCo2Rating = calculateRatingBy leastCommonBitWith 0

main = do
    contents <- getContents
    let diagnostics = lines $ contents
    let result = product $ [calculateO2Rating, calculateCo2Rating] <*> pure diagnostics

    putStr (show result)
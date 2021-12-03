import Data.Char
import Data.List
import Data.Function

type Binary = [Char]
type Bit = Char
type Bits = [Bit]

mostCommonBit :: Bits -> Bit
mostCommonBit = head . maximumBy (compare `on` length) . pairToList . partition (== '0')
    where pairToList = (\(x, y) -> [x, y])

binaryToInt :: Binary -> Int
binaryToInt = foldl (\acc x -> acc * 2 + digitToInt x) 0

complement :: Binary -> Binary
complement = map (\x -> if x == '1' then '0' else '1')

extractBits = transpose

main = do
    contents <- getContents
    let diagnostics = lines $ contents
    let gammaBin = map mostCommonBit . extractBits $ diagnostics
    let epsilonBin = complement $ gammaBin
    let result = product . map binaryToInt $ [gammaBin, epsilonBin]

    putStr (show result)
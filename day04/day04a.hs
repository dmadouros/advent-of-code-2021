import Data.List

checkWinner board = horizontal || vertical
    where horizontal = or . map (\row -> and . map (\square -> snd square) $ row) $ board 
          vertical   = or . map (\row -> and . map (\square -> snd square) $ row) . transpose $ board 

checkWinners boards = or . map checkWinner $ boards 

parseNumbers :: [[Char]] -> [String]
parseNumbers records = words . map (\x -> if x == ',' then ' ' else x) . head $ records

f :: [[[(String, Bool)]]] -> [String] -> [[[(String, Bool)]]]
f xs [] = xs
f xs records = f (xs ++ [board]) remainder
    where board = map (\l -> map (\x -> (x, False)) l) $ map words $ take 5 records
          remainder = drop 5 records

updateBoards number boards = map (\board -> map (\row -> map (\square@(v, marked) -> if v == number then (v, True) else square) row) board) $ boards

tick number boards = updateBoards number boards

play True prev _ boards = (prev, (find checkWinner boards))
play isWinner _ (number:numbers) boards = play (checkWinners boards') number numbers boards'
    where boards' = updateBoards number boards

sumUnmarked (Just board) = sum . map stringToInt . map fst . filter (\(v, marked) -> not marked) . concat $ board

stringToInt :: String -> Int
stringToInt = read

main = do
  contents <- getContents
  let records = lines $ contents
  let (_:boards) = records
  let numbers = parseNumbers records

  let remainder = filter (not . null) boards
  let boards = f [] remainder

  let (number, winner) = play False "0" numbers boards
  let unmarkedSum = sumUnmarked winner
  let answer = unmarkedSum * (stringToInt number)

  putStr (show answer)

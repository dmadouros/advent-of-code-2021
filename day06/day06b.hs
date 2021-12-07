tick [a,b,c,d,e,f,g,h,i] = [b,c,d,e,f,g,h + a,i,a]

play n = (!! n) . iterate tick

count a = length . filter (== a)

buckets = [0..8]

main = do
    input <- getContents
    let fish = read $ "[" ++ input ++ "]" :: [Integer]
    let histogram = map ((flip count) fish) $ buckets
    
    print . sum . play 256 $ histogram

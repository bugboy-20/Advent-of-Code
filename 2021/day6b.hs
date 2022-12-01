{--
nday :: Int -> [Int] -> [Int]
nday n p = if n==0 then p else nday (n-1) (pesiday p)

pesiday :: [Int] -> [Int]
pesiday (n:l) = if n==0 then 8:6:pesiday l else (n-1):pesiday l 
pesiday [] = []
--}
getpesci :: [Int] -> [(Int,Int)] -> [(Int,Int)]
getpesci n ((a,b):l) = if n==a then (a,b+1):l else getpesci n l
getpesci n [] = (n,1):[]


nday :: Int -> [(Int,Int)] -> [Int]
nday n p = if n==0 then p else nday (n-1) (pesiday p)

pesiday :: [(Int,Int)] -> [(Int,Int)]
pesiday (n:l) = if fst n /= 0 then (fst n+1,snd n):pesiday l else eliminaDoppioni (6,snd n):(8, snd n):l

eliminaDoppioni :: [(a,b)] -> [(a,b)]
eliminaDoppioni (n:l) = if 


toint :: [String] -> [Int]
toint = map read

main = do
    line <- getContents
    let fishes = toint $ wordsWhen (==',') $ head $ lines line
    print $ length $ nday 256 fishes

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

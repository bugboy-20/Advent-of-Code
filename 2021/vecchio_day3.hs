getGammaN :: Int -> [String] -> Int
getGammaN n (s:ss) = (if s !! (n-1) == '1' then 1 else -1)+getGammaN n ss
getGammaN n [] = 0

getGamma :: Int -> [String] -> [Int]
getGamma n s = if n/=0 then (getGammaN n s) : (getGamma (n-1) s) else []

gamma :: [String] -> [Int]
gamma s = getGamma (length s) s


main = do
    line <- getContents
    print $ gamma $ lines line


toN :: [String] -> [Int]
toN [] = []
toN (n:l) = read n : toN l

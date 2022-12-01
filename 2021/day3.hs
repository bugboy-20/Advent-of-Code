import Data.Array

hInput :: [String] -> [Array Int Char]
hInput = map --RICORDA CHE È MAPPATO AL CONTRARIO
      (\ s
         -> array (1, length s)
              $ zip (reverse [1 .. length s]) s)
gammaRate :: [Array Int Char] -> [Int]
gammaRate a = map (\n -> if n>0 then 1 else if n<0 then 0 else 32) $ gammaContatore a
gammaContatore :: [Array Int Char] -> [Int]
gammaContatore ars = map (gammaContatore' ars) [1..length $ head ars]
gammaContatore' :: [Array Int Char] -> Int -> Int
gammaContatore' ars i = foldr (\a b->b +(if (a!i) == '1' then 1 else -1)) 0 ars

esplilonRate a = map (\i -> if i==1 then 0 else 1) $ gammaRate a

bin :: [Int] -> Int
bin l = bin' l 1 where
    bin' [] _ = 0
    bin' (n:l) e = n*e + bin' l (2*e)

main = do
    l <- hInput . lines <$> readFile "./input3"
        {--
    print $ gammaRate l
    print $ esplilonRate l
    print $ bin (gammaRate l) * bin (esplilonRate l)
    --}
    print $ bin ( dsa $ head $ filtro l (>))
    print $ bin ( dsa $ head $ filtro l (<))
    print $ bin ( dsa $ head $ filtro l (>)) * bin ( dsa $ head $ filtro l (<))
    return ()

filtro :: [Array Int Char] -> (Int -> Int -> Bool) -> [Array Int Char]
filtro arrs = filtro' arrs (length $ head arrs)

filtro' :: [Array Int Char] -> Int -> (Int -> Int -> Bool) -> [Array Int Char]
filtro' a 0 _ = a
filtro' a i op = if length a == 1 then a else filtro' (filter (\m -> m!i == (if gammaContatore' a i `op` 0 then '1' else '0')) a) (i-1) op


--pfiltro' a 0 _ = return a
--pfiltro' a i op = print a >> (if length a == 1 then return a else pfiltro' (filter (\m -> m!i == (if gammaContatore' a i `op` 0 then '1' else '0')) a) (i-1) op)

-- concatena && Char -> Int
dsa :: Array Int Char -> [Int]
dsa a = dsa' (length a) a where
    dsa' 0 a = []
    dsa' n a = dsa' (n-1) a ++ [if a!n == '1' then 1 else 0]

goupp3 [] = []
goupp3 (a:b:c:l) = [a,b,c]:goupp3 (b:c:l)
goupp3 (a:b:[]) = []

sum3 :: [[Int]] -> [Int]
sum3 [] = []
sum3 (n:l) = sum n : sum3 l


aumento n [] = False
aumento n (m:l) = n<m

contaAumenti [] = 0
contaAumenti (n:l) = (if aumento n l then 1 else 0) + contaAumenti l

main = do
    line <- getContents
    print $ contaAumenti $ map sum $ goupp3 $ toN $ lines line


toN :: [String] -> [Int]
toN [] = []
toN (n:l) = read n : toN l

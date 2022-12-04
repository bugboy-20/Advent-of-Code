import Data.List
import Data.Char
import Data.List.Split (splitWhen)

input file = do
  f <- readFile file
  return $ lines f


(#>) f (a,b) = (f a, f b)

--parse :: String -> ((Int,Int),(Int,Int))
  {--
parse s = let ((a,b),(c,d)) = parse' s
         in ((digitToInt a, digitToInt b),(digitToInt c, digitToInt d))
        where 
          group (a:b:c:d:s) = ((a,b),(c,d))
          parse' = group . filter isNumber
--}
toIntervalPair :: ((Int,Int),(Int,Int)) -> ([Int],[Int])
toIntervalPair ((a,b),(c,d)) = ([a..b],[c..d])

linefstar :: ([Int],[Int]) -> Bool
linefstar p@(a,b)
  | f p == [] = False
  | otherwise = f p == a || f p == b 

  where 
    f (a,b) = intersect a b

fstar = length . filter (==True) . map (linefstar . toIntervalPair . parse)

sstar = length . filter (not . null) . map ( (uncurry intersect) . toIntervalPair . parse)

break' c s = let (a,b) = break c s
            in (a, tail b)

parse :: String -> ((Int,Int),(Int,Int))
parse s = ((read a, read b),(read c, read d))
  where
    ((a,b),(c,d)) = parse' s

parse' l = f '-' #> (a,b)
  where
    f c = break' (==c)
    (a,b) = f ',' l

main = do
  l <- input "day4"
  print $ fstar l
  print $ sstar l

import Control.Monad
import Text.Parsec
import Data.List

int = do
  i <- many digit
  return (read i :: Int)

lineParser = do
  x1 <- int
  char ','
  y1 <- int
  string " -> "
  x2 <- int
  char ','
  y2 <- int
  return ((x1,y1),(x2,y2))

readInput :: IO [((Int,Int),(Int,Int))]
readInput = do
  fileLines <- lines <$> readFile "input5"
  return $ map (\l ->
    case parse lineParser "stdin" l of
      Right a -> a
      ) fileLines

cordToList :: ((Int,Int),(Int,Int)) -> [(Int,Int)]
cordToList ((x1,y1),(x2,y2)) 
  | x1==x2 = [(x1,y) | y <- range y1 y2]
  | y1==y2 = [(x,y1) | x <- range x1 x2] 
  | abs (x1-x2) == abs (y1-y2) = [(x1 + i * signum (x2 - x1), y1 + i * signum (y2 - y1)) | i <- [0..(abs (x1-x2))]] 
  | otherwise = []

range a b
  | a<b = [a..b]
  | b<a = [b..a]

punti = concatMap cordToList <$> readInput
summary = map length . group . sort <$> punti
sol = length . filter (>1) <$> summary
  {--
intersections =
  let a = filter (not . null) . map cordToList <$> readInput
   in runIntersect <$> a where
     runIntersect (h:t) = (filter (not .null) . map (intersect h)) t : runIntersect t
     runIntersect [] = []

-- sol = foldr ((+) . length ) 0 <$> intersections
-- sol = sum . map (sum .length) . filter (not . null) <$> intersections
sol = sum . map (sum . map length) . filter (not . null) <$> intersections
--}

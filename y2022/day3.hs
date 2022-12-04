import Data.List as L
import Data.Map hiding (splitAt,map)

input = readFile


splitHalf :: [a] -> ([a],[a])
splitHalf s = splitAt n s
	where
      n = length s `div` 2

errorE (a,b) = head $ intersect a b 

priority = fromList $ zip ['a'..'z'] [1..26] ++ zip ['A'..'Z'] [27..52]

eval c = priority ! c

fstar :: [String] -> Int
fstar = sum . map (\s -> eval (errorE . splitHalf $ s))
groupsOf :: Int -> [a] -> [[a]]
groupsOf n as
  | n <= 0 = []
  | otherwise = go as
  where
    go [] = []
    go bs = xs : go rest
      where
        (xs, rest) = L.splitAt n bs

sstar = sum . map (eval . head . L.foldl intersect ['A'..'z']) . groupsOf 3
main = do
	l <- L.filter (not . L.null) . lines <$> input "day3"
	print $ fstar l
	print $ sstar l



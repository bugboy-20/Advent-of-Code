{-# LANGUAGE TupleSections #-}
import Data.List
import Data.List.Split

type Tabella = [[(Int,Bool)]]

strToNs :: String -> [Int]
strToNs = map read . words

nTobola :: String -> [(Int,Bool)]
nTobola w = map (,False) $ strToNs w

mktab :: [String] -> [[[(Int,Bool)]]]
mktab = map (map nTobola) . filter (not . null) . splitWhen (=="")

main = do
    fl <- lines <$> readFile "input4"
    let nums = read $ '[':(head fl ++ [']']) :: [Int]

    print $ mktab $ tail fl
    --print $ bingo nums $ mktab $ tail fl
    print $ bingoPerdente nums $ mktab $ tail fl
    return ()

bingo [] _ = -888
bingo (n:l) t = 
    let tab = map (tabEstrazione n) t
     in case unaVincente tab of
          Just r -> risposta r n
          Nothing -> bingo l tab

bingoPerdente :: [Int] -> [Tabella] -> Int
bingoPerdente [] _ = -324
bingoPerdente (n:l) t = 
    let tab = filter (not . tabVincente) $ map (tabEstrazione n) t 
     in if length tab == 1 then bingo l tab else bingoPerdente l tab

tabEstrazione :: Int -> Tabella -> Tabella
tabEstrazione n = map (map (\e -> if fst e == n then (n,True) else e))

unaVincente :: [Tabella] -> Maybe Tabella
unaVincente [] = Nothing
unaVincente (t:tt) = if tabVincente t then Just t else unaVincente tt

tabVincente :: Tabella -> Bool
tabVincente t = lineaCompleta t || lineaCompleta ( transpose t) where
    lineaCompleta [] = False
    lineaCompleta (l:t) = lineaCompleta' l || lineaCompleta t where
        lineaCompleta' [] = True
        lineaCompleta' ((_,True):l) = lineaCompleta' l
        lineaCompleta' ((_,False):l) = False


risposta :: Tabella -> Int -> Int
risposta t n = bollo False t * n

bollo :: Bool -> Tabella -> Int
bollo b l = sum $ map fst $ filter (\a -> snd a==b) $ concat l

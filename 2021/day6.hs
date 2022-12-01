import Data.Array
import Data.List

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn b s = case dropWhile b s of
                [] -> []
                s' -> w : splitOn b s'' 
                    where (w, s'') = break b s'


getInput = map (\e -> (head e, length e)) . group . sort . map (\n -> read n:: Int) . splitOn (==',') <$> readFile "input6"

lanternFish = (//) (listArray (0,8) $ repeat 0) <$> getInput
    {--
d :: IO [Int]
d = map (\n -> read n:: Int) . splitOn (==',') <$> readFile "input6"
   --}

days 0 f = f
days n f = days (n-1) $ aggiorna f
    where
        aggiorna f =
            let
            a = (map (\ (a,b) -> (a-1,b)) $ assocs f)
            h = (snd . head) a
            arr' = (f // tail a) 
             in arr' // [(6,arr'!6 + h),(8,arr'!8 +h)]
            
sol n = sum . days n <$> lanternFish

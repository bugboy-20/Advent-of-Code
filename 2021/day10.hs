data T = Int | Char deriving Show

-- n == 'Char' || ... generico
eqor n [] = Nothing
eqor n (m:l) = if n==m then Just n else eqor n l

parentesi '(' l i = if (l!!i) ==')' then parentesi (head l) l (i+1) else if ugulianze/=Nothing then ugulianze else parentesi (head l) l ()
    where
        ugulianze (l!!i) "]}>"






















{--
parentesi :: Char -> [Char] -> Maybe Char
parentesi c [] = Nothing 
parentesi '(' (n:l) = if n == ']' || n=='}' || n=='>' then Just n else parentesi n l
parentesi '[' (n:l) = if n == ')' || n=='}' || n=='>' then Just n else parentesi n l
parentesi '{' (n:l) = if n == ')' || n==']' || n=='>' then Just n else parentesi n l
parentesi '<' (n:l) = if n == ')' || n==']' || n=='}' then Just n else parentesi n l
parentesi _ (n:l) = parentesi n l
--}
punti :: (Maybe Char) -> Int
punti Nothing  = 0
punti (Just ')') = 3
punti (Just ']') = 57
punti (Just '}') = 1197
punti (Just '>') = 25137

    {--
main = do
    line <- getContents

    print $ sum $ map (\x -> punti $ parentesi (head x) $ tail x) $ lines line
--}

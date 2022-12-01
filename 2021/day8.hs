

sol1 = length . 
    filter ((\l -> l==2 || l==4 || l==3 || l==7) . length) . 
        concatMap (words . tail . dropWhile (/='|')) . 
            lines <$> readFile "input8"

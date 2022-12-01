data Manovra = Manovra Direzione Int deriving Show
data Direzione = Forward | Down | Up deriving Show
data Posizione = Posizione X Z Aim deriving Show
type X = Int
type Z = Int
type Aim = Int
makeManovra :: String -> Manovra
makeManovra s = Manovra (makeDirezione $ head $ words s) (read $ head $ tail $ words s)

makeDirezione :: String -> Direzione
makeDirezione "forward" = Forward
makeDirezione "down" = Down
makeDirezione "up" = Up
--makeDirezione _ = Nothing 

listManovre :: [String] -> [Manovra]
listManovre = map makeManovra

calcolaPosizione :: Posizione -> [Manovra] -> Posizione
calcolaPosizione (Posizione x z a) ((Manovra Up n):ms) = calcolaPosizione (Posizione x z (a-n)) ms
calcolaPosizione (Posizione x z a) ((Manovra Down n):ms) = calcolaPosizione (Posizione x z (a+n)) ms
calcolaPosizione (Posizione x z a) ((Manovra Forward n):ms) = calcolaPosizione (Posizione (x+n) (z+(n*a)) a) ms
calcolaPosizione p [] = p

risultato :: Posizione -> Int
risultato (Posizione x z a) = x*z

main = do
    line <- getContents
    print $ calcolaPosizione (Posizione 0 0 0) (listManovre $ lines line)
    print $ risultato $ calcolaPosizione (Posizione 0 0 0) (listManovre $ lines line)

{--
toN :: [String] -> [Int]
toN [] = []
toN (n:l) = read n : toN l
--}

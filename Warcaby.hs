import DrzewoGry (DrzewoGry, Gra(Gra, aktPlansza, kolorRuchu, notacja)
                 , poczatkoweDrzewoGry, wyciagnijStan, negamax, losowyRuch)
import Plansza (Plansza, Kolor(Czarny, Bialy), planszaDoStringa, przeciwny)
import System.Environment (getArgs)
import System.Random (randomIO)
import Data.Tree (Tree(Node))
import Data.List (find)

type Decydent = DrzewoGry -> IO DrzewoGry

graOutput :: (DrzewoGry -> DrzewoGry) -> DrzewoGry -> IO DrzewoGry
graOutput algorytmDecyzyjny drzewoGry = do
    let noweDrzewo@(Node gra _) = algorytmDecyzyjny drzewoGry
    putStrLn $ notacja gra
    return noweDrzewo
graInput :: DrzewoGry -> IO DrzewoGry
graInput drzewoGry@(Node _ las) = do
    ruch <- getLine
    let noweDrzewo = find ((ruch ==) . notacja . wyciagnijStan) las
    case noweDrzewo of
        Just drzewo -> return drzewo
        Nothing     -> putStrLn "Niepoporawny ruch" >> graInput drzewoGry
graIO :: DrzewoGry -> [Gra] -> Decydent -> Decydent -> IO ()
graIO (Node stan@(Gra _ kolor _) []) _ _ _ = print stan >> putStrLn (show (przeciwny kolor) ++ " wygrywa!")
graIO drzewoGry@(Node stan _) poprzednieStany aktualnyDecydent nastepnyDecydent 
    | trzyPowt  = print stan >> putStrLn "Remis: Ulozenie planszy powtorzylo sie trzykrotnie"
    | otherwise = do 
                    print stan
                    noweDrzewo <- aktualnyDecydent drzewoGry
                    graIO noweDrzewo (stan:poprzednieStany) nastepnyDecydent aktualnyDecydent
                  where ilosc a = scanl (\acc x -> if x==a then acc+1 else acc) 0
                        trzyPowt = any (>=3) $ ilosc stan poprzednieStany

graNegamax :: Int -> Decydent
graNegamax = graOutput . negamax
graLosowa :: Int -> Decydent
graLosowa = graOutput . losowyRuch

parse :: [String] -> IO (Decydent, Decydent)
parse []          = parse ["losowo", "losowo"]
parse (uno:[])    = parse [uno, "losowo"]
parse (uno:dos:_) = do
    bialy <- decydent uno
    czarny <- decydent dos
    return (bialy, czarny)

decydent :: String -> IO Decydent
decydent "input" = return graInput
decydent "losowo" = return . graLosowa =<< randomIO
decydent (n:"_negamax")   = return $ graNegamax (read [n])
decydent (n:m:"_negamax") = return $ graNegamax (read [n,m])
decydent _ = error "Zle argumenty"

main :: IO ()
main = do
    (bialy, czarny) <- parse =<< getArgs
    graIO poczatkoweDrzewoGry [] bialy czarny
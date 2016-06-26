module DrzewoGry
( DrzewoGry
, Gra(Gra, aktPlansza, kolorRuchu, notacja)
, wyciagnijStan
, poczatkoweDrzewoGry
, negamax
, losowyRuch
) where

import Plansza(przeciwny, poczatkowaPlansza, planszaDoStringa,
    Pozycja, Plansza, Warcab(Warcab), Kolor(Czarny, Bialy), Ranga(Damka, Zwykly))
import Ruchy(Ruch, generujRuchy, przesunWarcaba, jestWarcab, ruchDoNotacji)
import qualified Data.Map as Map (foldrWithKey)
import qualified Data.Tree as Tree (unfoldTree)
import Data.Tree (Tree(Node))
import Data.List (maximum, maximumBy, sortOn)
import Data.Ord (comparing)

type DrzewoGry = Tree Gra
data Gra = Gra {aktPlansza :: Plansza, kolorRuchu :: Kolor, notacja :: String}
instance Eq Gra where -- potrzebne do sprawdzenia remisu
    (Gra plansza1 kolor1 _) == (Gra plansza2 kolor2 _) = plansza1 == plansza2 && kolor1 == kolor2
instance Show Gra where
    show (Gra plansza _ _) = planszaDoStringa plansza

wyciagnijStan :: DrzewoGry -> Gra
wyciagnijStan (Node gra _) = gra
stworzDrzewoGry :: Gra -> DrzewoGry
stworzDrzewoGry stanPoczatkowy = 
    let las (Gra plansza kolor _) = do
            ruch <- generujRuchy plansza kolor
            return $ Gra (przesunWarcaba ruch plansza) (przeciwny kolor) (ruchDoNotacji ruch)
    in Tree.unfoldTree (\stan -> (stan, las stan)) stanPoczatkowy

poczatkoweDrzewoGry :: DrzewoGry
poczatkoweDrzewoGry = stworzDrzewoGry $ Gra poczatkowaPlansza Bialy undefined

-- ewaluacje
maxWartosc :: Int
maxWartosc = maxBound
minWartosc :: Int
minWartosc = -maxWartosc
wartPrzegranej :: Int
wartPrzegranej = -10^5
znakWarcaba :: Warcab -> Int
znakWarcaba (Warcab kolor _) = znakDla kolor
znakDla :: Kolor -> Int
znakDla Bialy = 1
znakDla Czarny = -1

wartoscWarcaba :: Warcab -> Int
wartoscWarcaba (Warcab _ Zwykly) = 50
wartoscWarcaba (Warcab _ Damka) = 200
-- po 1: odklada na jak najpozniej wyjscie z ostatniego rzedu
-- po 2: premia za zblizanie sie do damkowansu
wartoscPostepu :: Warcab -> Pozycja -> Int
wartoscPostepu (Warcab Czarny Zwykly) (_, rzad) =
    wartoscPostepu (Warcab Bialy Zwykly) (0, (9-rzad))
wartoscPostepu (Warcab Bialy Zwykly) (_, rzad)
    | rzad > 6  = 5
    | rzad > 4  = 3
    | rzad > 2  = 1
    | rzad > 1  = 0
    | otherwise = 10
wartoscPostepu (Warcab _ Damka) _ = 0
wartoscObszaru :: Pozycja -> Int
wartoscObszaru pos
    | odlOdSrodka pos 2 = 10
    | odlOdSrodka pos 3 = 5
    | otherwise         = 0
    where odlOdSrodka (x, y) odl = all ((<odl) . odlOdSrodka') [x, y]
          odlOdSrodka' x = abs $ 4.5 - (fromIntegral x)
wartoscGrupy :: Warcab -> Pozycja -> Plansza -> Int
wartoscGrupy (Warcab kolor _) (x, y) plansza =
    let pktZaSasiada = 2
    in sum [pktZaSasiada | x' <- [x+1, x-1], y' <- [y+1, y-1], jestWarcab kolor (x', y') plansza]
wartoscPlanszy :: Plansza -> Int
wartoscPlanszy plansza = 
    let wartoscCalosciowa pos warcab  
            = wartoscWarcaba warcab 
            + wartoscPostepu warcab pos
            + wartoscGrupy warcab pos plansza
            + wartoscObszaru pos 
        zliczWart pos warcab acc = (znakWarcaba warcab)*(wartoscCalosciowa pos warcab) + acc
    in Map.foldrWithKey zliczWart 0 plansza

--algorytmy ruchowe
negamax :: Int -> DrzewoGry -> DrzewoGry
negamax glebokosc (Node (Gra _ kolor _) nastepneStany) =
    fst $ alfaBeta nastepneStany glebokosc minWartosc maxWartosc (znakDla kolor) wartPoczatkowa
    where wartPoczatkowa = (error "negamax: Pusta lista", minWartosc)
negamax' :: DrzewoGry -> Int -> Int -> Int -> Int -> Int
negamax' (Node (Gra plansza _ _) _) 0 _ _ znak = znak * (wartoscPlanszy plansza)
negamax' (Node _ nastepneStany) glebokosc alfa beta znak = 
    snd $ alfaBeta nastepneStany glebokosc alfa beta znak (undefined, (wartPrzegranej-glebokosc))
alfaBeta :: [DrzewoGry] -> Int -> Int -> Int -> Int -> (DrzewoGry, Int) -> (DrzewoGry, Int)
alfaBeta [] _ _ _ _ optymalny = optymalny
alfaBeta (stan:stany) glebokosc alfa beta znak optymalny 
    | alfa >= beta = optymalny -- odciecie a-b
    | otherwise    = alfaBeta stany glebokosc nowaAlfa beta znak nowyOptymalny
    where rozwazany = (stan, -(negamax' stan (glebokosc-1) (-beta) (-alfa) (-znak)))
          nowyOptymalny = maximumBy (comparing snd) [rozwazany, optymalny]
          nowaAlfa = max alfa $ snd rozwazany

losowyRuch :: Int -> DrzewoGry -> DrzewoGry
losowyRuch _ (Node _ []) = error "losowyRuch: Pusta lista"
losowyRuch seed (Node _ nastepneStany) = nastepneStany !! (seed `mod` (length nastepneStany))
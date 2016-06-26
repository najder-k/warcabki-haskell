module Ruchy
( Ruch(Ruch, RuchZlozony)
, generujRuchy
, przesunWarcaba
, jestWarcab
, ruchDoNotacji
) where

import Plansza (przeciwny, pokazPozycje, 
    Pozycja, Plansza, Warcab(Warcab, kolor), Kolor(Czarny, Bialy), Ranga(Damka, Zwykly))
import Data.Maybe (fromJust)
import qualified Data.Map as Map (lookup, notMember, member, foldrWithKey, insert, delete, adjust, filter)

data Ruch = Ruch Pozycja Pozycja | RuchZlozony {wyciagnijRuchy :: [Pozycja]} deriving (Eq, Show)

ruchDoNotacji :: Ruch -> String
ruchDoNotacji (Ruch start koniec) = 
    pokazPozycje start ++ "-" ++ pokazPozycje koniec
ruchDoNotacji (RuchZlozony (start:_:reszta)) = 
    pokazPozycje start ++ "x" ++ ruchDoNotacji (RuchZlozony reszta)
ruchDoNotacji (RuchZlozony [koniec]) = pokazPozycje koniec

generujRuchy :: Plansza -> Kolor -> [Ruch]
generujRuchy plansza kolorRuchu
    | zbijajace /= [] = tylkoNajdluzsze zbijajace
    | otherwise       = wszystkie ruchyZwykle
    where zbijajace = wszystkie ruchyZbijajace
          warcaby   = warcabyKoloru kolorRuchu plansza
          wszystkie ruchyTypu = 
            Map.foldrWithKey (\pos warcab acc -> ruchyTypu warcab pos plansza ++ acc) [] warcaby
tylkoNajdluzsze :: [Ruch] -> [Ruch]
tylkoNajdluzsze ruchy =
    filter ((maxDlugosc ==) . length . wyciagnijRuchy) ruchy
    where maxDlugosc = maximum $ map (length . wyciagnijRuchy) ruchy

ruchyZwykle :: Warcab -> Pozycja -> Plansza -> [Ruch]
ruchyZwykle (Warcab kolor Zwykly) start plansza = 
    [Ruch start koniec | koniec <- ruchy kolor, ruchPoprawny plansza koniec]
    where ruchy Czarny = ruchyZwykle' (-) start
          ruchy Bialy = ruchyZwykle' (+) start
ruchyZwykle _ start plansza =
    [Ruch start koniec | koniec <- ruchy]
    where ruchy = ruchyDamka start >>= takeWhile (ruchPoprawny plansza)
ruchyZbijajace :: Warcab -> Pozycja -> Plansza -> [Ruch]
ruchyZbijajace warcab start plansza = 
    [RuchZlozony (start:ruch) | ruch <- ruchy, ruch /= []]
    where ruchy = ruchyZbijajace' warcab start (usunWarcaba start plansza)

ruchyZwykle' :: (Int -> Int -> Int) -> Pozycja -> [Pozycja]
ruchyZwykle' f (x, y) = 
    [(x + 1, y `f` 1), (x - 1, y `f` 1)] 
ruchyDamka :: Pozycja -> [[Pozycja]]
ruchyDamka (x, y) = 
    [[(x+signX*m, y+signY*m) | m <- [1..7]] | signX <- [-1, 1], signY <- [-1, 1]]
ruchyZbijajace' :: Warcab -> Pozycja -> Plansza -> [[Pozycja]]
ruchyZbijajace' warcab@(Warcab kolor Zwykly) (x, y) plansza
    | dopuszczalne /= [] = 
        [zbij:lad:reszta | (zbij, lad) <- dopuszczalne, reszta <- nastepneRuchy zbij lad]
    | otherwise          = [[]] -- konieczne jako zakonczenie listy ruchow
    where wszystkie =
              [((x+signX, y+signY), (x+2*signX, y+2*signY)) | signX <- [-1, 1], signY <- [-1, 1]]
          dopuszczalne = filter warunkiZbicia wszystkie
          warunkiZbicia (zbij, lad) = ruchPoprawny plansza lad && jestCoZbic kolor plansza zbij
          nastepneRuchy zbij lad = ruchyZbijajace' warcab lad (usunWarcaba zbij plansza)
-- bicie damkowe (brzydkie ale dziala)
ruchyZbijajace' warcab pos plansza
    | ruchy /= [] = ruchy
    | otherwise   = [[]]
    where ruchy = filter (/= []) $ ruchyDamka pos >>= trasaBiciaDamki warcab plansza
trasaBiciaDamki :: Warcab -> Plansza -> [Pozycja] -> [[Pozycja]]
trasaBiciaDamki warcab plansza pozycje
    | jestDoZbicia && wolnePola /= [] = [zbij:lad:reszta | lad <- wolnePola, reszta <- nastepneRuchy lad]
    | otherwise                       = [[]]
    where mozeZbijane = dropWhile (ruchNaWolnePole plansza) $ takeWhile ruchWPlanszy pozycje
          (zbij:ladowanie) = mozeZbijane
          jestDoZbicia = mozeZbijane /= [] && jestCoZbic (kolor warcab) plansza zbij
          wolnePola = takeWhile (ruchNaWolnePole plansza) ladowanie
          nastepneRuchy lad = ruchyZbijajace' warcab lad (usunWarcaba zbij plansza)

-- filtrowanie i warunki
tenSamKolor :: Kolor -> Warcab -> Bool
tenSamKolor kolorSzukany (Warcab kolorWarcaba _) = kolorSzukany == kolorWarcaba
warcabyKoloru :: Kolor -> Plansza -> Plansza
warcabyKoloru kolor = Map.filter (tenSamKolor kolor)
ruchPoprawny :: Plansza -> Pozycja -> Bool
ruchPoprawny plansza ruch = ruchWPlanszy ruch && ruchNaWolnePole plansza ruch
-- and $ [ruchWPlanszy, ruchNaWolnePole plansza] <*> [ruch]
ruchWPlanszy :: Pozycja -> Bool
ruchWPlanszy (x, y) = and [x>=1, y>=1, x<=8, y<=8]
ruchNaWolnePole :: Plansza -> Pozycja -> Bool
ruchNaWolnePole plansza (x, y) = Map.notMember (x, y) plansza
jestCoZbic :: Kolor -> Plansza -> Pozycja -> Bool
jestCoZbic kolor plansza pos = jestWarcab (przeciwny kolor) pos plansza
jestWarcab :: Kolor -> Pozycja -> Plansza -> Bool
jestWarcab kolor pos plansza =
    maybe False (tenSamKolor kolor) $ Map.lookup pos plansza

-- modyfikacja planszy
przesunWarcaba :: Ruch -> Plansza -> Plansza
przesunWarcaba (Ruch posStartowa posKoncowa) plansza =
    sprawdzKoronowanie warcab posKoncowa 
    $ dodajWarcaba posKoncowa warcab 
    $ usunWarcaba posStartowa plansza
    where warcab = fromJust $ Map.lookup posStartowa plansza
przesunWarcaba (RuchZlozony (posStartowa:doZbicia:posKoncowa:[])) plansza = 
    usunWarcaba doZbicia 
    $ przesunWarcaba (Ruch posStartowa posKoncowa) plansza
przesunWarcaba (RuchZlozony (posStartowa:doZbicia:posKoncowa:resztaRuchow)) plansza =
    przesunWarcaba (RuchZlozony $ posKoncowa:resztaRuchow) 
    $ usunWarcaba doZbicia 
    $ przesunWarcabaBezKoronacji (Ruch posStartowa posKoncowa) plansza
przesunWarcabaBezKoronacji :: Ruch -> Plansza -> Plansza
przesunWarcabaBezKoronacji (Ruch posStartowa posKoncowa) plansza =
    dodajWarcaba posKoncowa warcab 
    $ usunWarcaba posStartowa plansza
    where warcab = fromJust $ Map.lookup posStartowa plansza
usunWarcaba :: Pozycja -> Plansza -> Plansza
usunWarcaba = Map.delete
dodajWarcaba :: Pozycja -> Warcab -> Plansza -> Plansza
dodajWarcaba = Map.insert
sprawdzKoronowanie :: Warcab -> Pozycja -> Plansza -> Plansza
sprawdzKoronowanie (Warcab Czarny Zwykly) pos@(_, 1) plansza = 
    Map.adjust (\_ -> Warcab Czarny Damka) pos plansza 
sprawdzKoronowanie (Warcab Bialy Zwykly) pos@(_, 8) plansza = 
    Map.adjust (\_ -> Warcab Bialy Damka) pos plansza 
sprawdzKoronowanie _ _ plansza = plansza
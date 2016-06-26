module Plansza 
( Pozycja
, Plansza
, Kolor(Czarny, Bialy)
, Ranga(Damka, Zwykly)
, Warcab(Warcab, kolor)
, pokazPozycje
, poczatkowaPlansza
, planszaDoStringa
, przeciwny --kolor
) where

import qualified Data.Map as Map (fromList, lookup)
import Data.Map (Map)
import Data.List (intersperse)

type Pozycja = (Int, Int)
type Plansza = Map Pozycja Warcab
data Kolor = Czarny | Bialy deriving (Eq, Show)
data Ranga = Zwykly | Damka deriving (Eq, Show)
data Warcab = Warcab {kolor :: Kolor, ranga :: Ranga} deriving (Eq, Show)

poczatkowaPlansza :: Plansza
poczatkowaPlansza = Map.fromList $ genPos [1..3] (Warcab Bialy Zwykly) ++ genPos [6..8] (Warcab Czarny Zwykly)

pokazPozycje :: Pozycja -> String
pokazPozycje (x, y) = ['a'..'h'] !! (x-1) : (show y) -- notacja kurnikowa
-- pokazPozycje (x, y) = show $ (8-y) * 4 + (x+1) `quot` 2 -- notacja zwykla

warcabToStr :: Maybe Warcab -> Char
warcabToStr' (Warcab Czarny Zwykly) = 'c'
warcabToStr' (Warcab Bialy Zwykly) = 'b'
warcabToStr' (Warcab Czarny Damka) = 'C'
warcabToStr' (Warcab Bialy Damka) = 'B'
warcabToStr = maybe '-' warcabToStr'

przeciwny :: Kolor -> Kolor
przeciwny Czarny = Bialy
przeciwny Bialy = Czarny

granePola :: Pozycja -> Bool
granePola (x, y) = even (x + y) -- oba parzyste, lub oba nieparzyste

genPos :: [Int] -> Warcab -> [(Pozycja, Warcab)]
genPos rows warcab = [((x, y), warcab) | x <- [1..8], y <- rows, granePola (x, y)]

planszaDoStringa :: Plansza -> String
planszaDoStringa plansza = 
    odstepy . ocyfrz $ unlines [[if granePola (x, y) then pokazWarcaba (x, y) else ' ' | x <- [1..8]] | y <- [8,7..1]]
    where ocyfrz plansza = unlines $ (zipWith (:) ['8','7'..'1'] $ lines plansza) ++ ['/':['a'..'h']]
          odstepy plansza = unlines $ map (intersperse ' ') $ lines plansza
          pokazWarcaba (x, y) = warcabToStr $ Map.lookup (x, y) plansza

main = putStrLn $ planszaDoStringa poczatkowaPlansza
# Warcabki
## Opis uruchomienia
Główny moduł: Warcaby.hs

Pierwszy argument programu to sterowanie białymi, drugi to sterowanie czarnymi.

Możliwe argumenty: "input", "losowo", "x_negamax" (gdzie x to głębokosc przeszukiwania drzewa gry)

np. 
- "Warcaby input 5_negamax" to gra przyjmująca ruchy dla białych i wybierajaca ruchy dla czarnych, negamaxem o głębokości 5
- "Warcaby 10_negamax 3_negamax"  dla czarnych negamaxem o głębokości 3
- "Warcaby 10_negamax losowo" to gra wybierająca ruchy dla białych negamaxem o głębokości 10 i wybierajaca ruchy dla czarnych losowo
	
Domyślnie (gdy któryś z argumentow nie będzie podany) ruchy wybierane są losowo

## Działanie
Gra wg zasad warcab klasycznych.

Przyjowane ruchy są walidowane. Gdyby podany ruch nie znajdował się w drzewie gry, do konsoli zostanie wpisany komunikat "Niepoprawny ruch" i program jeszcze raz będzie czekał na wpisanie.

Gdy dany układ planszy powtórzy się trzykrotnie, gra jest przerywana, a w konsoli zostanie wypisany komunikat o remisie.

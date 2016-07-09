
Autor : Katarzyna Burczyk

uruchamianie: plik wykonywalny checkers.exe
		lub kompilacja: ghc --make "Main.hs" i uruchomienie powsta³ego pliku


zawartoœæ kolejnych modu³ów:

1. main:
	- obs³uga gry
2. minmax:
	- okreœlanie stanu planszy,  wyliczanie wartoœci materialnej figur graczy
	- tworzenie drzewa gry
	- algorytm minmax, s³u¿¹cy wirtualnemu graczowi do wyboru ruchu
3. moves:
	- badanie i obs³uga mo¿liwoœci ruchów
4. board:
	- reprezentacja planszy, pionków
	- mo¿liwoœæ manipulacji plansz¹
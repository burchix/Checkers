
Autor : Katarzyna Burczyk

uruchamianie: plik wykonywalny checkers.exe
		lub kompilacja: ghc --make "Main.hs" i uruchomienie powsta�ego pliku


zawarto�� kolejnych modu��w:

1. main:
	- obs�uga gry
2. minmax:
	- okre�lanie stanu planszy,  wyliczanie warto�ci materialnej figur graczy
	- tworzenie drzewa gry
	- algorytm minmax, s�u��cy wirtualnemu graczowi do wyboru ruchu
3. moves:
	- badanie i obs�uga mo�liwo�ci ruch�w
4. board:
	- reprezentacja planszy, pionk�w
	- mo�liwo�� manipulacji plansz�
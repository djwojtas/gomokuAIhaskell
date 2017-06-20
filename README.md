__Kod znajduje się w hask/Main.hs__

Dostępna jest gra Player vs CPU (domyślnie jako funkcja main) oraz CPU vs CPU (wystarczy usunąć 1 komentarz i dodać na dole programu aby zmienić funkcję main na ten tryb)

__Watości funkcji do wersji zbudowanej i uruchamianej z poziomu ghci__

Optymalna wartość głębokości drzewa i jego gałęzienia dla wersji uruchamianej z ghci to odpowiednio 4 i 3, jest to wartość domyślna i wymaga 5-10 sekund na ruch w ghci. 372 linijka kodu dla takiego ustawienia wygląda następująco:
  makeMove board pawn = setPawn board (getMaxScoreField board pawn $ zipMaxWithFields 4 3 board pawn) pawn
Wartości optymalne dla wersji zbudowanej to 6 i 4 (ok. 4-6 sekund na ruch). 372 linijka dla takiego ustawienia:
  makeMove board pawn = setPawn board (getMaxScoreField board pawn $ zipMaxWithFields 6 4 board pawn) pawn

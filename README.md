# clj-nkjp

_English:_ Clojure utilities to process the [National Corpus of
Polish][1] (NKJP) data.

_Polish:_ Narzędzia w Clojure do przetwarzania danych 
[Narodowego Korpusu Języka Polskiego][1].

 [1]: http://nkjp.pl/

Na ten projekt składają się następujące przestrzenie nazw:

 * _clj-nkjp.tei_: Konwertuje anotację morfoskładniową milionowego
   podkorpusu NKJP (pliki `ann_morphosyntax.xml`) na s-wyrażenia
   clojurowe.

   Wyniki konwersji (3889 plików .clj skompresowanych programem
   7zip; 10 MB) można pobrać [stąd][2].
   
 [2]: http://danieljanus.pl/nkjp-clj.7z

## License / Licencja

WTFPL (zob. plik LICENSE).

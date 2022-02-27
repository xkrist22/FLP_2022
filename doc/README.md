# Funkcionální projekt: BKG-2-CNF
- Autor: Jiří Křištof, xkrist22@stud.fit.vutbr.cz
- Předmět: FLP (Funkcionální a logické programování)
- Datum: 22. 2. 2022

## Úvod
V rámci projektu je implementován algoritmus pro převod bezkontextové vlastní gramatiky (BKG) na bezkontextovou gramatiku v Chomského normální formě (CNF). Vlastní gramatika je gramatika bez zbytečných symbolů, cyklů a epsilon-pravidel. 

## Implementační detaily
Program je implementován s využitím funkcionálního jazyka Haskell. Pro překlad je přiložen soubor `Makefile`, který příkazem `make` provede překlad programu kompilátorem `ghc` a příkazem `make test` spustí testy. Vstupní a výstupní testovací soubory jsou uloženy ve složce `test`. Pro testování je přiložen shell skript `test.sh` v hlavním adresáři projektu. 
Výstup programu (gramatika v CNF) je vždy vypsána na `stdout`. 

### Parametry příkazové řádky
- -i: na `stdout` je vypsána vnitřní reprezentace vstupní BKG,
- -1: na `stdout` je vypsána BKG bez jednoduchých pravidel,
- -2: na `stdout` je vypsána vstupní gramatika v CNF,
- vstupní soubor: volitelný parametr, povinně uváděn až za výše uvedenými parametry, není-li uveden, pak je vstup očekáván na `stdin`. 

V případě kombinace více parametrů definujících mód programu je program ukončen a není provedena žádná akce. V případě, že není uveden žádný argument, vstup je očekáván na stdout a je proveden převod na CNF. Není možné uvést samotný vstupní soubor bez módu programu.

### Formát vstupu
Bezkontextová gramatika je čtveřive obsahující konečnou množinu neterminálů, konečnou množinu terminálů, konečnou množinu přepisovacích pravidel a počáteční neterminál. V rámci projektu je množina neterminálů podmnožinou velkých písmen a množina terminálů podmnožinou malých písmen. Vstupní soubor (či vstup na `stdout` pak musí splňovat následující formát:
	seznam všech neterminálů oddělených čárkami
	seznam všech terminálů oddělených čárkami
	počáteční neterminál
	pravidlo 1
	pravidlo 2
	...
	pravidlo n
přičemž levá strana pravidel obsahuje právě 1 neterminál, pravá strana pak kombinaci terminálů a neterminálů (vyjma slova epsilon). Obě strany pravidel jsou odděleny šipkou "->". V rámci vstupu je možné používat libovolně bílé znaky (mezery, tab), jejich použití nemá vliv na výslednou gramatiku.

### Vnitřní reprezentace bezkontextové gramatiky
Při načítání BKG ze specifikovaného zdroje je tato uložena do čtveřice, kde jednotlivé elementy mají následující významy:
- První element obsahuje pole `[Char]` všech neterminálních symbolů,
- Druhý element obsahuje pole `[Char]` všech terminálních symbolů,
- Třetí element je znak obsahující počáteční neterminální symbol,
- Posledním elementem je pole dvojic, kde každá dvojice reprezentuje 1 pravidlo BKG; prvním elementem je neterminál levé strany, druhý element poté obsahuje pravou stranu pravidla. 

### Návratové kódy
Program využívá tyto návratové kódy:
- `0`: program úspěšně provedl převod BKG do CNF
- `1`: vstupní soubor neexistuje
- `2`: kombinace více vstupních parametrů, 
- `3`: vstupní soubor neodpovídá používanému formátu

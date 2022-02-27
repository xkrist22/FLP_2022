build: src
	ghc -Wall -o flp21-fun src/main.hs src/Cnf.hs src/Easy.hs src/Parser.hs

run:
	./flp21-fun

test:
	./test.sh
	
clean:
	cd src;rm *.hi *.o 

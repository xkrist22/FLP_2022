build: src
	ghc -Wall -o flp21-fun src/main.hs src/Cnf.hs src/Easy.hs src/Parser.hs

run:
	./flp21-fun

test:
	chmod +x tests/test.sh
	./tests/test.sh
	
clean:
	cd src;rm *.hi *.o 

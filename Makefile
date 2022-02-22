build: src
	ghc -o flp21-fun src/main.hs

run:
	./flp21-fun

test:
	./test.sh
	

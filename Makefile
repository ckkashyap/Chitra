all:
	ghc -hide-package monads-fd --make Main

clean:
	@find . -name "*.hi" -exec rm -f {} \;
	@find . -name "*.o" -exec rm -f {} \;
	rm -f Main

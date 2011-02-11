all:
	ghc -hide-package monads-fd --make Main

run:
	ghci -hide-package monads-fd Main.hs

clean:
	@find . -name "*.hi" -exec rm -f {} \;
	@find . -name "*.o" -exec rm -f {} \;
	rm -f Main

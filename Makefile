all:
	ghc --make Main

clean:
	@find . -name "*.hi" -exec rm -f {} \;
	@find . -name "*.o" -exec rm -f {} \;
	rm -f Main

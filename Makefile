all:
	ghc --make Main

clean:
	rm -f {.,Chitra,RFB}/*.{o,hi}

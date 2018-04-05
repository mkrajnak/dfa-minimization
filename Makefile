# DKA-2-MKA, project for FLP course at FIT BUT
# Martin Krajnak, xkrajn02@stud.fit.vurbr.cz

all:
	ghc --make dka-2-mka.hs
clean:
	rm -rf dka-2-mka *.o  *.hi *.zip
zip:
	zip -r flp-fun-xkrajn02.zip dka-2-mka.hs Makefile Readme.md

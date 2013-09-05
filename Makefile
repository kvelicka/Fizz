all: Viewer FP2floats
clean:
	rm *.o *.hi Viewer FP2floats

FP2floats: FP2floats.hs
	ghc --make -O2 FP2floats
Viewer: *.hs 
	ghc --make -O2 Viewer

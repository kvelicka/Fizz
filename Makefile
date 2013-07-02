all: haskell C
clean:
	rm *.o *.hi DownSample downsampleC upsampleC txt2dat vtxt2dat uvwtxt2dat Viewer
uncompile:
	rm *.o *.hi Viewer
haskell: DownSample Viewer
C: downsampleC upsampleC txt2dat vtxt2dat uvwtxt2dat

DownSample: DownSample.hs AstroData.hs
	ghc --make -O2 DownSample
Viewer: *.hs fixedPrecisionC.o
	ghc --make -O2 Viewer fixedPrecisionC.o

downsampleC: downsampleC.o fixedPrecisionC.o
	gcc -o $@ downsampleC.o fixedPrecisionC.o -lm
upsampleC: upsampleC.o fixedPrecisionC.o
	gcc -o $@ upsampleC.o fixedPrecisionC.o -lm
txt2dat: txt2dat.o fixedPrecisionC.o
	gcc -o $@ txt2dat.o fixedPrecisionC.o -lm
vtxt2dat: vtxt2dat.o fixedPrecisionC.o
	gcc -o $@ vtxt2dat.o fixedPrecisionC.o -lm
uvwtxt2dat: uvwtxt2dat.o fixedPrecisionC.o
	gcc -o $@ uvwtxt2dat.o fixedPrecisionC.o -lm

txt2dat.o: txt2dat.c fixedPrecisionC.h
	gcc -c txt2dat.c -O2
vtxt2dat.o: vtxt2dat.c fixedPrecisionC.h
	gcc -c vtxt2dat.c -O2
uvwtxt2dat.o: uvwtxt2dat.c fixedPrecisionC.h
	gcc -c uvwtxt2dat.c -O2
downsampleC.o: downsampleC.h downsampleC.c fixedPrecisionC.h
	gcc -c downsampleC.c -O2
fixedPrecisionC.o: fixedPrecisionC.h fixedPrecisionC.c
	gcc -c fixedPrecisionC.c -O2


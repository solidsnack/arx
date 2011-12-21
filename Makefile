
arx: arx.hs
	ghc -outputdir ./tmp --make -O2 ./arx.hs -o arx

./tmp/arx.ubuntu: arx.hs
	ghc -outputdir ./tmp --make -O2 ./arx.hs -no-link
	./ubuntu/link.sh ./tmp/arx.ubuntu
	strip ./tmp/arx.ubuntu

doc:
	cd ./docs && make blessed

clean:
	rm -rf ./tmp ./arx

dist/build/arx/arx: cabal

./tmp/arx.osx: dist/build/arx/arx
	mkdir -p ./tmp
	cp ./dist/build/arx/arx ./tmp/arx.osx
	strip ./tmp/arx.osx

TMPXTools=dist/build/arx/arx-tmp/System/Posix/ARX/TMPXTools.hi \
	  dist/build/arx/arx-tmp/System/Posix/ARX/TMPXTools.o \
	  dist/build/System/Posix/ARX/TMPXTools.p_hi \
	  dist/build/System/Posix/ARX/TMPXTools.p_o \
	  dist/build/System/Posix/ARX/TMPXTools.hi \
	  dist/build/System/Posix/ARX/TMPXTools.o
$(TMPXTools): ./model-scripts/tmpx.sh
	rm -f $@

Main=./dist/build/arx/arx-tmp/Main.hi \
     ./dist/build/arx/arx-tmp/Main.o
$(Main): ./docs/blessed/arx.txt
	rm -f $@

cabal: $(TMPXTools) $(Main)
	cabal configure && cabal build


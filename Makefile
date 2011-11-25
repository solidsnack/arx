
arx: arx.hs
	ghc -outputdir ./tmp --make -O2 ./arx.hs -o arx

clean:
	rm -rf ./tmp ./arx

dist/build/arx/arx:
	cabal configure && cabal build

./tmp/arx: dist/build/arx/arx
	mkdir -p ./tmp
	cp ./dist/build/arx/arx ./tmp/arx
	strip ./tmp/arx


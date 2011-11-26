
arx: arx.hs
	ghc -outputdir ./tmp --make -O2 ./arx.hs -o arx

./tmp/arx.ubuntu: arx.hs
	ghc -outputdir ./tmp --make -O2 ./arx.hs -no-link
	./ubuntu/link.sh ./tmp/arx.ubuntu
	strip ./tmp/arx.ubuntu

clean:
	rm -rf ./tmp ./arx

dist/build/arx/arx:
	cabal configure && cabal build

./tmp/arx.osx: dist/build/arx/arx
	mkdir -p ./tmp
	cp ./dist/build/arx/arx ./tmp/arx.osx
	strip ./tmp/arx.osx


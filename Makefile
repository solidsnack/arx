
arx: arx.hs
	ghc -outputdir ./tmp --make -O2 ./arx.hs -o arx

clean:
	rm -rf ./tmp ./arx

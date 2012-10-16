.PHONY: doc clean cabal ubuntu-build-deps

tag = arx-$(shell ./bin/dist tag)

ifeq (Darwin,$(shell uname))
  tagged = tmp/arx.cabal
else
  tagged = tmp/arx.custom
endif

this_platform: tmp/dist/$(tag)/arx

tmp/dist/$(tag)/arx: $(tagged)
	mkdir -p tmp/dist/$(tag)
	mv -f $< $@

tmp/dist/%/arx.gpg: tmp/dist/%/arx
	gpg --use-agent --detach-sign $<

tmp/dist/%/arx.sha: tmp/dist/%/arx
	shasum --portable --algorithm 512 $< > $@

tarballs: $($(wildcard tmp/dist/*/arx):%/arx=%.tbz)

tmp/dist/%.tbz: d = $(@:tmp/dist/%.tbz=%)
tmp/dist/%.tbz: tmp/dist/%/arx tmp/dist/%/arx.gpg tmp/dist/%/arx.sha
	tar cjf $d.tbz -C tmp/dist $d


arx: arx.hs doc
	ghc -outputdir tmp --make -O2 arx.hs -o arx

tmp/arx.custom: libs = $(shell dist/so2a4hs statics arx)
tmp/arx.custom: arx dist/so2a4hs
	ghc -outputdir tmp --make -O2 arx.hs -o $@ \
	 -optl-Wl,--whole-archive \
	  $(libs:%=-optl%) \
	 -optl-Wl,--no-whole-archive
	strip $@

ubuntu-build-deps:
	env DEBIAN_FRONTEND=noninteractive aptitude install -y \
	  python-sphinx cabal-install
	cabal install --only-dependencies

tmp/arx.cabal: dist/build/arx/arx
	mkdir -p tmp
	cp dist/build/arx/arx $@
	strip $@

dist/build/arx/arx: cabal

cabal:
	cabal configure --disable-executable-profiling --disable-library-profiling
	cabal build

doc:
	cd docs && make blessed

clean:
	rm -rf tmp arx dist/build


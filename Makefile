GHCFLAGS=-Wall -XNoCPP -fno-warn-name-shadowing -XHaskell98 -O2
HLINTFLAGS=-u -XHaskell98 -XNoCPP -i 'Use camelCase' -i 'Use String' -i 'Use string literal' -i 'Use list comprehension'
VERSION=0.1.1

.PHONY: all clean doc install

all: report.html doc dist/build/haskades/haskades dist/haskades-$(VERSION).tar.gz

install: dist/build/haskades/haskades
	cabal install

report.html: haskades.hs Records.hs
	-hlint $(HLINTFLAGS) --report $^

doc: dist/doc/html/haskades/index.html README

README: haskades.cabal
	tail -n+$$(( `grep -n ^description: $^ | head -n1 | cut -d: -f1` + 1 )) $^ > .$@
	head -n+$$(( `grep -n ^$$ .$@ | head -n1 | cut -d: -f1` - 1 )) .$@ > $@
	-printf ',s/        //g\n,s/^.$$//g\nw\nq\n' | ed $@
	$(RM) .$@

dist/doc/html/haskades/index.html: dist/setup-config haskades.hs Records.hs
	-cabal haddock --hyperlink-source --executables

dist/setup-config: haskades.cabal
	cabal configure

MustacheTemplates.hs: HaskadesBinding.hs.mustache haskades_run.cpp.mustache signals.h.mustache Records.hs
	mustache2hs -m Records.hs HaskadesBinding.hs.mustache Template \
		haskades_run.cpp.mustache Template signals.h.mustache Template > MustacheTemplates.hs

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist dist-ghc MustacheTemplates.hs

dist/build/haskades/haskades: haskades.cabal dist/setup-config haskades.hs Records.hs MustacheTemplates.hs
	cabal build --ghc-options="$(GHCFLAGS)"

dist/haskades-$(VERSION).tar.gz: haskades.cabal dist/setup-config README haskades.hs Records.hs MustacheTemplates.hs
	cabal check
	cabal sdist

SOURCES=$(shell find src -name "*.hs")

all: package

test:
	cabal configure --enable-tests && cabal build && cabal test

clean:
	find . -name "*~" -exec rm \{\} \;
	rm -rf dist

realclean: clean
	cabal sandbox delete

package: .cabal-sandbox dist/build/proto2graphql/proto2graphql

dist/build/proto2graphql/proto2graphql: cabal.sandbox.config $(SOURCES)
	cabal build

.cabal-sandbox: proto2graphql.cabal
	cabal install --only-dependencies --enable-tests

.cabal-sandbox/bin/%: .cabal-sandbox
	cabal install $*

cabal-install: .cabal-sandbox/bin/alex .cabal-sandbox/bin/happy

cabal.sandbox.config:
	cabal sandbox init

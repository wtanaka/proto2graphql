SOURCES=$(shell find src -name "*.hs")

all: package

test:
	cabal configure --enable-tests && cabal build && cabal test

clean:
	find . -name "*~" -exec rm \{\} \;
	rm -rf dist

realclean: clean
	cabal sandbox delete

package: cabal-install dist/build/proto2graphql/proto2graphql

dist/build/proto2graphql/proto2graphql: cabal.sandbox.config $(SOURCES)
	cabal build

.cabal-sandbox/bin/%: cabal.sandbox.config
	cabal install $*

cabal-install: .cabal-sandbox/bin/alex .cabal-sandbox/bin/happy \
		proto2graphql.cabal
	cabal install --only-dependencies --enable-tests

.cabal-sandbox cabal.sandbox.config:
	cabal sandbox init
	cabal sandbox add-source ../graphql-schema
	cabal sandbox add-source ../protocol-buffers
	cabal sandbox add-source ../protocol-buffers/hprotoc
	cabal update

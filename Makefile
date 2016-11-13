all: package

clean:
	find . -name "*~" -exec rm \{\} \;
	rm -rf dist

realclean: clean
	cabal sandbox delete

package: dist/build/proto2graphql/proto2graphql

dist/build/proto2graphql/proto2graphql: cabal.sandbox.config
	cabal install --only-dependencies
	cabal build

.cabal-sandbox cabal.sandbox.config:
	cabal sandbox init

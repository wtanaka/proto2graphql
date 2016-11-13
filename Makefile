all: cabal.sandbox.config

.cabal-sandbox cabal.sandbox.config:
	cabal sandbox init

format:
	fourmolu -i src/**/*.hs
install:
	cabal install --overwrite-policy=always
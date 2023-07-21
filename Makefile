format:
	fourmolu -i src/**/*.hs
install:
	cabal install --overwrite-policy=always --installdir=docker --install-method=copy 
image:
	docker build -t giacenza docker 
format:
	fourmolu -i src/**/*.hs
install:
	cabal install --overwrite-policy=always --installdir=docker --install-method=copy
image:
	strip docker/giacenza
	docker build -t paolino/giacenza:devel docker
	docker push paolino/giacenza:devel
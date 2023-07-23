format:
	fourmolu -i src
	fourmolu -i test
	fourmolu -i app
install:
	cabal install --overwrite-policy=always --installdir=docker --install-method=copy
image:
	strip docker/giacenza
	docker build -t paolino/giacenza:devel docker
	docker push paolino/giacenza:devel
serve:
	docker/giacenza serve 0.0.0.0 8080 ""
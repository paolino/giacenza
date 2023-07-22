from haskell:9.4.5-buster as build
RUN cabal update
COPY . /opt/giacenza
WORKDIR /opt/giacenza
RUN cabal install -j --installdir=. --install-method=copy

from debian:buster
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev
COPY --from=build /opt/giacenza/giacenza /usr/bin/giacenza
CMD ["giacenza"]

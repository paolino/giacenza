# giacenza media (balance average)

Compute the _balance average_ from the bank transaction statements as a CSV file.

The files should contain the _full_ history of transations, positive and negative.

The csv is expected to have 2 relevant column
  - ***Date*** The date of the transaction as "%Y-%m-%d" (i.e. 1970-10-22)
  - ***Amount*** The amount moved (positive is incoming), the format must be specified as argument

Name of columns and number format can be specified at request time.

## Build from source

- download haskell tools [GHCUp](https://www.haskell.org/ghcup/)
- fire `ghcup tui` and select ghc-9.4.5 as compiler
- > cabal install

## Run as a web server container from dockerhub locally

- need docker support from your operating system
- > docker run -p 8080:8080 paolino/giacenza giacenza serve 0.0.0.0 8080 ""


## Use the online instance

[giacenza](https://plutimus.com/giacenza)

## Help from the commandline
```
Usage: giacenza serve HOST PORT PREFIX

  Start the web server

Available options:
  HOST                     The host to bind to
  PORT                     The port to bind to
  PREFIX                   The prefix to add to the http calls
  -h,--help                Show this help text

```

## TODO

- [ ] Better error on CSV parsing issues
- [ ] Add multi year reporting by merging streams instead of streaming one result per year
- [ ] Use optionExt to have header and footer on --help
# giacenza media

Compute _giacenza media_ from some transactions statements as CSV files.

The files should contain the _full_ history of transations, positive and negative.

The csv is expected with header 2 relevant columns have to be there, their name must be specified as arguments
  - ***date*** The date of the transaction as "%Y-%m-%d" (i.e. 1970-10-22)
  - ***amount*** The amount moved (positive is incoming), the format must be specified as argument

## Install  

- download haskell tools [GHCUp](https://www.haskell.org/ghcup/)
- fire `ghcup tui` and select ghc-9.8.2 as compiler
- > cabal install --overwrite-policy=always

## Run 

> giacenza statements 2021 Data Importo european

## Help
```

Giacenza media

Usage: giacenza DIR YEAR DATE-NAME AMOUNT-NAME NUMBER-FORMAT

Available options:
  -h,--help                Show this help text
  DIR                      The directory with the statements
  YEAR                     The year for the giacenza
  DATE-NAME                The date field name
  AMOUNT-NAME              The amount field name
  NUMBER-FORMAT            The number format, european or american
```

## TODO

- [ ] Better error on CSV parsing issues
- [ ] Add multi year reporting by merging streams instead of streaming one result per year
- [ ] Use optionExt to have header and footer on --help
# giacenza media

Compute _giacenza media_ from some transactions statements as CSV files.

The files should contain the _full_ history of transations, positive and negative.

The csv is expected with header and the 2 relevant columns should be 
  - ***Date*** The date of the transaction as "%Y-%m-%d"
  - ***Amount*** The amount moved (positive is incoming), comma is decimal separator , dot as thousands separator

## Install  

download haskell tools [GHCUp](https://www.haskell.org/ghcup/)

> cabal install --overwrite-policy=always

## Run 

> giacenza statements 2020 Data Importo



## Help
```

Giacenza media

Usage: giacenza DIR YEAR DATE-NAME AMOUNT-NAME

Available options:
  -h,--help                Show this help text
  DIR                      The directory with the statements
  YEAR                     The year for the giacenza
  DATE-NAME                The date field name
  AMOUNT-NAME              The amount field name

```

## TODO

Support other number format
# Giacenza

Giacenza computes the **giacenza media** (average daily deposit) and **saldo**
(end-of-year balance) of a bank account from CSV transaction files.

## CSV requirements

- **Date format**: `YYYY-MM-DD` (ISO 8601, e.g. `2024-01-15`)
- **Columns**: the file must have a header row; column names for date and amount
  are configurable in the app
- **Number format**: the decimal separator (dot or comma) is configurable in the
  app
- **Content**: include all movements for the account history you want to analyse

## Features

- Upload **multiple CSV files** and aggregate results across all of them
- Configure column mapping and number format once — settings are remembered via a
  cookie
- Results are grouped by year, showing giacenza media and saldo for each
- A **final report** row aggregates totals across all uploaded files

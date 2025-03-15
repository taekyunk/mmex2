
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mmex2

<!-- badges: start -->

<!-- badges: end -->

The goal of mmex is to make it easy to read information from Money
Manager EX (MMEX) database. MMEX is an excellent open source personal
finance manager and you can find and download from
[here](https://www.moneymanagerex.org/)

For instance, MMEX has the following benefits:

- It is freeware and open source
- It is available on multiple platform (e.g. PC, Linux, and Android).
- The database is in SQLite format

This package is built for my personal use to create reports of my
personal finance. For instance

- The package does not join all tables in MMEX.
- The package does not use investment accounts because I track them as
  bank accounts instead.

However, this package includes utility functions to

- list all tables
- get one table by name
- get category information as as table

So, you can pull tables of interest and join them properly by using the
table relationship described
[here](https://github.com/moneymanagerex/database)

## Note

- Only supports 1.6.2 or above
  - Note that the category structure is updated to be more flexible
- With older versions of MMEX, use this package instead [GitHub -
  taekyunk/mmex](https://github.com/taekyunk/mmex)
- Also, assume only two levels of category are used
  - Personally, I don’t see much use for deeply nested categories

## Author

Taekyun (TK) Kim (<taekyunk@gmail.com>)

## Installation

You can install the github version of mmex with:

``` r
library(devtools)
install_github("taekyunk/mmex2") 
```

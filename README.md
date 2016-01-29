# querysnmb

The querysnmb package provides functions to query the SNMB database in an
amicable manner, this is, the user does not need to know the database schema
or repeateadly perform annoying joins to have useful databases.

The package was developed using Roxygen and devtools, as described in [R Packages](http://r-pkgs.had.co.nz).

## Functions
The functions in this package use [dplyr](https://github.com/hadley/dplyr) to query the database, the advantage of
using them is that the query functions perform the necessary joins to have a
useful table of information, and provide several variables to filter the
results by.

## Installation

```
  install.packages("devtools")
  devtools::install_github("tereom/querysnmb")
```

## Future code improvements
To optimize the code there are some simple improvements awaiting:

1. Instead of importing the tables to the R environment and applying algebraic operations (filter, joins, selects, etc..) in R, one could perform the operations in the dbms (dbms optimizes the order of the operations and we import smaller datasets) and later import the results to R. For example, when using the dplyr package in R, the strategy would be to only use `collect` for final results. 

2. If we are to import tables, and perform joins and filters in the R environment, one should first filter, for example, in queryInvaders the inner joins follow the order Conglomerado -> Transecto -> Epecie\_invasora -> Archivo\_especie\_invasora, once we have the final data.frame we filter by Archivo_especie_invasora, thus, it will be better to first filter and reverse the order of the joins, this way we work with smaller data.frames for we are not carrying useless rows throught the sequence of joins.

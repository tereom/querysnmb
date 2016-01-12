# querysnmb

The querysnmb package provides functions to query the SNMB database in an
amicable manner, this is, the user does not need to know the database schema
or repeateadly perform annoying joins to have useful databases.

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

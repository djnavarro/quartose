
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quartose

<!-- badges: start -->

[![R-CMD-check](https://github.com/djnavarro/quartose/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/djnavarro/quartose/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/djnavarro/quartose/graph/badge.svg)](https://app.codecov.io/gh/djnavarro/quartose)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/quartose)](https://CRAN.R-project.org/package=quartose)
<!-- badges: end -->

When analyzing data sets in R, it is often convenient to wrap the
analysis within a quarto document for reporting purposes: containing all
the analysis components within a single easy-to-navigate HTML document
is generally a kindness for the reader. One consequence of this is that
sometimes you find yourself wanting to write code within an R code chunk
that will generate parts of the quarto document for you. For instance,
when iterating over many analyses within a single R chunk, you might
want to have that chunk generate the quarto section headers, tabsets and
so on. This is possible because the knitr engine (which evaluates the R
code chunks) allows you to generate “asis” output that will later be
captured by the quarto parser; if such output is formatted to look like
correct quarto syntax, it will be captured and translated to the
appropriate HTML.

The purpose of the quartose package is to provide some helper functions
to make this task a little easier. The reason for writing it is that
while it is conceptually straightforward to generate quarto syntax
within R, there are some practical issues in juggling knitr, quarto, and
R all at once if you want the resulting document to look clean. The goal
in writing the quartose package is to handle some of those niceties,
making it a little easier to work programmatically within quarto
documents.

## Installation

You can install the development version of quartose from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("djnavarro/quartose")
```

## Example

The core idea is illustrated with this example:

``` r
library(quartose)

# define a quarto tabset
tabs <- quarto_tabset(
  content = list(tab1 = 1:5, tab2 = "hello"), 
  title = "My tabs", 
  level = 2
)

# base::print() outputs a simple summary
print(tabs)
#> <quarto_tabset>
#> • content: <list>
#> • title: My tabs
#> • names: tab1 tab2
#> • level: 2

# knitr::knit_print() outputs quarto syntax
knitr::knit_print(tabs)
#> 
#> 
#> ## My tabs
#> 
#>  
#> 
#> 
#> ::: {.panel-tabset}
#> 
#>  
#> 
#> 
#> ### tab1
#> 
#>  
#> <pre> 
#> [1] 1 2 3 4 5 
#> </pre> 
#> 
#> 
#> ### tab2
#> 
#>  
#> <pre> 
#> [1] "hello" 
#> </pre> 
#> 
#> 
#> ::: 
#> 
#> 
```

## Related work

It is of course inevitable that the moment you start work on something
and talk to people about it, it turns out that other people have done
similar work. So far I’ve discovered two other approaches to the
problem. They’re not identical or equivalent to what I’m attempting to
do here, but they’re definitely thinking about the same ideas. To that
end, I’d encourage anyone thinking about quartose to also look into
these packages:

- The [quartabs](https://github.com/sayuks/quartabs) package, by Sasaki
  Yusuke
- The [qreport](https://hbiostat.org/r/qreport/) package, by Frank
  Harrell

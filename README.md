
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quartose

<!-- badges: start -->

[![R-CMD-check](https://github.com/djnavarro/quartose/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/djnavarro/quartose/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/djnavarro/quartose/graph/badge.svg)](https://app.codecov.io/gh/djnavarro/quartose)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

When analysing data sets in R, it is often convenient to wrap the
analysis within a quarto document for reporting purposes: containing all
the analysis components within a single easy-to-navigate HTML document
is generally a kindness for the reader. One consequence of this,
however, is that sometimes you find yourself wanting to write code
*within* an R code chunk that will generate parts of the quarto document
for you. For instance, when iterating over many analyses within a single
R chunk, you might want to have that chunk generate the quarto section
headers, tabsets and so on. This is possible because the knitr engine
(which evaluates the R code chunks) allows you to generate “asis” output
that will later be captured by the quarto parser; if such output is
formatted to look like correct quarto syntax, it will be captured and
translated to the appropriate HTML.

The purpose of the quartose package is to provide some helper functions
to make this task a little easier. The reason for writing it is that
while it is *conceptually* straightfoward to generate quarto syntax
within R, there are some practical issues in juggling knitr, quarto, and
R all at once if you want the resulting document to look clean. That’s
what I wrote quartose for: I want the nice, clean outputs that you get
when you take care of all those little nuances, but also I don’t want to
have to add *lots* of tiresome formatting code into my analysis
documents. So I decided to suck it up, write myself a little package
that handles this for me, so that I never have to think about it again.

## Installation

You can install the development version of quartose from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("djnavarro/quartose")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(quartose)
## basic example code
```

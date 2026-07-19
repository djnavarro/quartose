# Print a quarto object

Prints a quarto object. When calling
[`knitr::knit_print()`](https://rdrr.io/pkg/knitr/man/knit_print.html)
on a quarto object, the relevant
[`format()`](https://rdrr.io/r/base/format.html) method is called first,
and the formatted version is printed to the document. When calling
[`print()`](https://rdrr.io/r/base/print.html), a summary of the object
structure is printed.

## Usage

``` r
# S3 method for class 'quarto_object'
knit_print(x, ...)

# S3 method for class 'quarto_object'
print(x, ...)
```

## Arguments

- x:

  A quarto object.

- ...:

  Other arguments (ignored).

## Value

[`knitr::knit_print()`](https://rdrr.io/pkg/knitr/man/knit_print.html)
invisibly returns `NULL`; [`print()`](https://rdrr.io/r/base/print.html)
invisibly returns the quarto object itself.

## Details

There are two print methods supplied for quarto objects, one for
[`base::print()`](https://rdrr.io/r/base/print.html) and another for
[`knitr::knit_print()`](https://rdrr.io/pkg/knitr/man/knit_print.html).
The regular print method behaves similarly to any other print method: it
prints a summary of the object to the R console, and invisibly returns
the object itself.

When
[`knitr::knit_print()`](https://rdrr.io/pkg/knitr/man/knit_print.html)
is called on a quarto object, the behavior is quite different. The
object is first passed to
[`format()`](https://rdrr.io/r/base/format.html), which constructs the
required quarto syntax, then the object is printed to the document (or
console, if called interactively) using the appropriate syntax. In this
case, the function invisibly returns `NULL`.

Note that [`print()`](https://rdrr.io/r/base/print.html) is
console-only: it never writes quarto syntax into the rendered document,
even when called from within a code chunk. Its console summary is
written via
[`cli::cli_text()`](https://cli.r-lib.org/reference/cli_text.html),
which routes through [`message()`](https://rdrr.io/r/base/message.html)
rather than [`cat()`](https://rdrr.io/r/base/cat.html) when not attached
to an interactive terminal (as is the case during `quarto render`). Some
output formats hide `message`/`warning` chunk output by default (e.g.
`revealjs` presentations), so a stray
[`print()`](https://rdrr.io/r/base/print.html) call left in a chunk may
appear to produce nothing at all under those formats, even though the
object was evaluated correctly. To emit the actual quarto syntax into
the document, use
[`knitr::knit_print()`](https://rdrr.io/pkg/knitr/man/knit_print.html)
inside a chunk with the `results: asis` option.

## Examples

``` r
# a quarto_section object
sec <- quarto_section("A level-two header", level = 2L)
 
# base::print() displays a summary of the object 
print(sec)
#> <quarto_section>
#> • title: A level-two header
#> • level: 2

# knitr::knit_print() displays the rendered quarto syntax
knitr::knit_print(sec) 
#> 
#> 
#> ## A level-two header
#> 
#>  

# a quarto_span object
spn <- quarto_span("This is underlined", class = "underline")

print(spn)
#> <quarto_span>
#> • content: This is underlined
#> • class: underline
#> • sep:

knitr::knit_print(spn)
#> [This is underlined]{.underline} 
 
```

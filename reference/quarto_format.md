# Format a quarto object

Creates a formatted representation of a quarto object in a form suitable
for printing. When calling
[`knitr::knit_print()`](https://rdrr.io/pkg/knitr/man/knit_print.html)
on a quarto object, the relevant
[`format()`](https://rdrr.io/r/base/format.html) method is called first,
and the formatted version is printed to the document. Note that the base
[`print()`](https://rdrr.io/r/base/print.html) method for quarto objects
does not call [`format()`](https://rdrr.io/r/base/format.html).

## Usage

``` r
# S3 method for class 'quarto_section'
format(x, ...)

# S3 method for class 'quarto_tabset'
format(x, ...)

# S3 method for class 'quarto_div'
format(x, ...)

# S3 method for class 'quarto_span'
format(x, ...)

# S3 method for class 'quarto_markdown'
format(x, ...)

# S3 method for class 'quarto_group'
format(x, ...)
```

## Arguments

- x:

  A quarto object.

- ...:

  Other arguments (ignored).

## Value

A formatted quarto object. For `quarto_section`, `quarto_span`, and
`quarto_markdown` objects, the formatted output is always a string
(character vector of length 1). For `quarto_tabset` and `quarto_group`
objects, the output is always a list whose elements are either strings
or plot objects. For `quarto_div` objects, the output is a string unless
`content` includes a graphics object, in which case it is a list whose
elements are either strings or plot objects, exactly as for
`quarto_tabset`.

## Details

The intent behind the [`format()`](https://rdrr.io/r/base/format.html)
methods for quarto objects is to create a ready-to-print representation
of that is almost identical to what will be printed into the quarto
document when
[`knitr::knit_print()`](https://rdrr.io/pkg/knitr/man/knit_print.html)
is called. Because of this, the formatted version of a quarto object is
a string or a list of strings, but it may also include plot objects that
have not yet been rendered. The resulting representation isn't always
very pretty, though it is generally fairly readable.

**Escaping policy.**
[`quarto_tabset()`](https://quartose.djnavarro.net/reference/quarto_object.md)
accepts arbitrary R objects as `content` and captures their default
printed representation (via
[`knitr::knit_print()`](https://rdrr.io/pkg/knitr/man/knit_print.html)/[`capture.output()`](https://rdrr.io/r/utils/capture.output.html))
to display inside each tab. Most objects'
`knit_print()`/[`print()`](https://rdrr.io/r/base/print.html) methods
write this representation as a side effect (for example, an `lm`
object's `Call:` summary, or a data frame's default print), and that
captured text may incidentally contain `<` or `>` (for example, a
tibble's `<fct>` column-type tag), which would otherwise be parsed as an
unrecognized HTML tag by quarto/pandoc and silently dropped from the
rendered document. To prevent this, `format.quarto_tabset()` escapes `<`
and `>` to `&lt;`/`&gt;` in that captured text before it is written out,
and wraps it in `<pre>` tags. Some objects instead *return* their output
marked via
[`knitr::asis_output()`](https://rdrr.io/pkg/knitr/man/asis_output.html)
(class `"knit_asis"`) rather than printing it — this includes
`knitr::kable(format = "html")`, `flextable` objects, `gt` tables, and
most htmlwidget-like objects. For these, `format.quarto_tabset()`
detects the `"knit_asis"` class and emits the raw markup unescaped and
without a `<pre>` wrapper, so it is rendered as intended (a table,
widget, etc.) rather than displayed as literal text.
[`quarto_div()`](https://quartose.djnavarro.net/reference/quarto_object.md)
supports the same `"knit_asis"` passthrough for its `content`, emitted
unescaped for the same reason — but unlike
[`quarto_tabset()`](https://quartose.djnavarro.net/reference/quarto_object.md),
it validates content at construction time, so objects that neither print
like ordinary R objects nor return `"knit_asis"` output (e.g. a bare
list, a model object, or a number) are rejected up front rather than
falling back to captured/escaped text.
[`quarto_span()`](https://quartose.djnavarro.net/reference/quarto_object.md)
content is restricted by validation to character vectors (never
arbitrary captured print output), and
[`quarto_markdown()`](https://quartose.djnavarro.net/reference/quarto_object.md)
is explicitly meant to carry raw markdown/HTML through untouched —
neither of these is escaped.

## Examples

``` r
# formatted sections, spans and divs ----------------------------------
sec <- quarto_section("Header", level = 2L)
spn <- quarto_span("Content", class = "underline")
div <- quarto_div("Content", class = "content-margin")

format(sec)
#> [1] "\n\n## Header\n\n"

format(spn)
#> [1] "[Content]{.underline}"

format(div)
#> [1] "\n\n::: {.content-margin}\n\n Content \n\n:::\n\n"

# formatted tabsets ---------------------------------------------------
tbs <- quarto_tabset(
  content = list(tab1 = 1:10, tab2 = "hello"),
  title = "Header",
  level = 2L
)

format(tbs)
#> [[1]]
#> [1] "\n\n## Header\n\n"
#> 
#> [[2]]
#> [1] "\n\n::: {.panel-tabset}\n\n"
#> 
#> [[3]]
#> [1] "\n\n### tab1\n\n"
#> 
#> [[4]]
#> [1] "<pre>"
#> 
#> [[5]]
#> [1] " [1]  1  2  3  4  5  6  7  8  9 10"
#> 
#> [[6]]
#> [1] "</pre>"
#> 
#> [[7]]
#> [1] "\n\n### tab2\n\n"
#> 
#> [[8]]
#> [1] "<pre>"
#> 
#> [[9]]
#> [1] "[1] \"hello\""
#> 
#> [[10]]
#> [1] "</pre>"
#> 
#> [[11]]
#> [1] "\n\n::: \n\n"
#> 

# formatted groups and markdown ---------------------------------------

mkd <- quarto_markdown(list("- this is a", "- markdown list"), sep = "\n")
gps <- quarto_group(list(div, mkd))

format(mkd)
#> [1] "- this is a\n- markdown list"

format(gps)
#> [[1]]
#> [1] "\n\n::: {.content-margin}\n\n Content \n\n:::\n\n"
#> 
#> [[2]]
#> [1] "- this is a\n- markdown list"
#> 
```

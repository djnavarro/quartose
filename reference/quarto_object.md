# Dynamically generate quarto syntax

Define quarto objects for insertion into a document. Intended to be used
inside a quarto document, within a knitr code chunk with the
`results: asis` option set.

## Usage

``` r
quarto_section(title, level)

quarto_tabset(content, level, title = NULL, names = NULL)

quarto_div(content, class = NULL, sep = "")

quarto_span(content, class = NULL, sep = "")

quarto_group(content, sep = "")

quarto_markdown(content, sep = "")
```

## Arguments

- title:

  Character string specifying the text to use as a section title. For
  `quarto_section()` this is a required argument. For `quarto_tabset()`
  it is permitted to use `title = NULL`, in which case the tabset will
  be printed without a section header above it. This is the default
  behavior for tabsets.

- level:

  Numeric header level applied to section title or tabset names. The
  `level` argument must be a whole number between 1 and 6. Only relevant
  to quarto objects that produce section headings, specifically
  `quarto_section()` and `quarto_tabset()`.

- content:

  List or character vector containing content to be included within the
  quarto object. The expected format of the `content` argument differs
  slightly depending on which function is used. See the "details"
  section for more information.

- names:

  Character vector of names to be applied to the tabs in a tabset. Only
  relevant to `quarto_tabset()`. If `names = NULL`, the names will be
  taken from the names of the `content` argument.

- class:

  Character vector specifying CSS classes to be applied to the content.
  Only relevant to `quarto_div()` and `quarto_span()`. Defaults to
  `class = NULL`, in which case the formatted text written to the
  document will have a dummy CSS class "quartose-null" applied.

- sep:

  Character string specifying the separator to be used when merging
  content for printing to the document. Defaults to `sep = ""` for all
  functions.

## Value

These functions always return an object with parent S3 class
"quarto_object", in addition to a specific S3 class corresponding to the
function. For example, `quarto_section()` objects also possess the
"quarto_section" class.

## Details

The purpose of these functions is to allow the user to dynamically
generate quarto syntax from R. When used within a quarto document they
allow the user to generate callouts, margin text, tabsets, section
headers, and other kinds of quarto output. At the current state of
development the functionality is somewhat limited, discussed below.

The `quarto_*()` functions supplied by the quartose package have a
common design: argument values supplied by the user are stored
internally as a list, with only a minimum of processing done at the time
that the function is called. The object is assigned to two S3 classes,
the "quarto_object" shared by all objects, and a specific class
associated with the calling function. These objects can be inspected and
manipulated programmatically like any other R objects prior to printing.

When creating a quarto object, note that most `quarto_*()` functions
take a `content` argument, which differs slightly depending on the
context:

- For `quarto_section()` there is no \`content“ argument: section
  headers have titles, but they do not contain content.

- For `quarto_span()` the \`content“ argument *must* be a character
  vector, not a list.

- For `quarto_div()` the `content` argument is permitted to be a
  character vector or a list, but it will always be stored internally as
  a list. If the input is a list, it can contain other quarto objects,
  or graphics objects of the same kinds supported by `quarto_tabset()`
  (ggplot2/patchwork, base R recorded plots, grid grobs, lattice/trellis
  objects, or anything tagged via
  [`as_quarto_graphic()`](https://quartose.djnavarro.net/reference/as_quarto_graphic.md)).
  The intended use for this is a div that contains several spans, but it
  is not limited to this use case.

- For `quarto_tabset()` the `content` argument *must* be a list. The
  list elements can be any printable R object: each element of the list
  will appear in its own tab. Several kinds of graphics objects are
  auto-detected and rendered as images rather than as captured text:
  ggplot2 (and patchwork, which subclasses ggplot2) objects, base R
  recorded plots
  ([`grDevices::recordPlot()`](https://rdrr.io/r/grDevices/recordplot.html)),
  grid grobs, and lattice/trellis objects. If a graphics object from
  another package isn't auto-detected, wrap it with
  [`as_quarto_graphic()`](https://quartose.djnavarro.net/reference/as_quarto_graphic.md)
  to force this treatment. Rendering happens when
  [`knitr::knit_print()`](https://rdrr.io/pkg/knitr/man/knit_print.html)
  is called.

- For `quarto_markdown()` the `content` argument may be a character
  vector or a list of character vectors. The function will throw an
  error if other kinds of objects are passed via `content`.

- For `quarto_group()` the `content` argument *must* be a list, and all
  elements of the list must be quarto objects. The intended use of this
  function is simply to collect several quarto objects into a single
  group that will be printed all at the same time rather than
  sequentially.

Creating a quarto object only defines the data structure, it does not
perform any formatting. Similarly, if the object is printed using
[`print()`](https://rdrr.io/r/base/print.html), no formatting will be
applied. A brief summary of the data structure will be printed to the
console, no more. However, when
[`knitr::knit_print()`](https://rdrr.io/pkg/knitr/man/knit_print.html)
is called, the quarto object is first passed to the relevant
[`format()`](https://rdrr.io/r/base/format.html) method, which is
responsible for constructing the appropriate quarto syntax. Calling
[`format()`](https://rdrr.io/r/base/format.html) will return a character
vector or a list. If it returns a list all elements will either be
character strings with the appropriate quarto syntax, or a plot object
that has not yet been rendered. After formatting is applied the
[`knitr::knit_print()`](https://rdrr.io/pkg/knitr/man/knit_print.html)
method will pass the strings (or plots) to the document. For more detail
on the formatting and printing methods see
[`knit_print.quarto_object()`](https://quartose.djnavarro.net/reference/quarto_print.md)
and
[`format.quarto_object()`](https://quartose.djnavarro.net/reference/quarto_format.md).

## Examples

``` r
# quarto_section ------------------------------------------------------

sec <- quarto_section("A level-two header", level = 2L)

# quarto objects have two classes, a general purpose class shared by 
# all quarto objects, and a class specific to the function
class(sec) 
#> [1] "quarto_section" "quarto_object" 
 
# base::print() displays an abstract summary of the object 
print(sec)
#> <quarto_section>
#> • title: A level-two header
#> • level: 2

# knitr::knit_print() produces the rendered quarto syntax
knitr::knit_print(sec)
#> 
#> 
#> ## A level-two header
#> 
#>  

# quarto_span ---------------------------------------------------------

spn1 <- quarto_span("This is plain text")
spn2 <- quarto_span("This is underlined text", class = "underline")

print(spn1)
#> <quarto_span>
#> • content: This is plain text
#> • class:
#> • sep:

print(spn2)
#> <quarto_span>
#> • content: This is underlined text
#> • class: underline
#> • sep:

knitr::knit_print(spn1)
#> [This is plain text]{.quartose-null} 

knitr::knit_print(spn2)
#> [This is underlined text]{.underline} 

# quarto_div ----------------------------------------------------------

# quarto_div objects are flexible: they can take a character vector as
# the content argument, but can also take lists of other objects; note
# that internally the content is always represented as a list
div1 <- quarto_div("This is a callout note", class = "callout-note")
div2 <- quarto_div(
  content = list(
    quarto_span(content = "You can wrap multiple spans in a div so that"),
    quarto_span(content = "some text is highlighted", class = "mark"),
    quarto_span(content = "and some is underlined", class = "underline")
  ),
  class = c("column-margin", "callout-tip"),
  sep = " "
)

print(div1)
#> <quarto_div>
#> • content: <list>
#> • class: callout-note
#> • sep:

print(div2)
#> <quarto_div>
#> • content: <list>
#> • class: column-margin callout-tip
#> • sep:

knitr::knit_print(div1)
#> 
#> 
#> ::: {.callout-note}
#> 
#>  This is a callout note 
#> 
#> :::
#> 
#>  

knitr::knit_print(div2)
#> 
#> 
#> ::: {.column-margin .callout-tip}
#> 
#>  [You can wrap multiple spans in a div so that]{.quartose-null} [some text is highlighted]{.mark} [and some is underlined]{.underline} 
#> 
#> :::
#> 
#>  

# quarto_tabset -------------------------------------------------------

tbs <- quarto_tabset(list(tab1 = 1:10, tab2 = "hello"), level = 3L)

print(tbs)
#> <quarto_tabset>
#> • content: <list>
#> • title:
#> • names: tab1 tab2
#> • level: 3

knitr::knit_print(tbs)
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
#>  [1]  1  2  3  4  5  6  7  8  9 10 
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

# quarto_markdown -----------------------------------------------------

mkd <- quarto_markdown(list("- a markdown", "- list"), sep = "\n")

print(mkd)
#> <quarto_markdown>
#> • content: <list>
#> • sep:

knitr::knit_print(mkd)
#> - a markdown
#> - list 

# quarto_group --------------------------------------------------------

grp <- quarto_group(list(
  quarto_div("This is a callout note", class = "callout-note"),
  quarto_div("This is a callout tip", class = "callout-tip")
))

print(grp)
#> <quarto_group>
#> • content: <list>
#> • sep:

knitr::knit_print(grp)
#> 
#> 
#> ::: {.callout-note}
#> 
#>  This is a callout note 
#> 
#> :::
#> 
#>  
#> 
#> 
#> ::: {.callout-tip}
#> 
#>  This is a callout tip 
#> 
#> :::
#> 
#>  
```

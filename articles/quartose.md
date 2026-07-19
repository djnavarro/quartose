# quartose

``` r

library(knitr)
library(dplyr)
library(purrr)
library(ggplot2)
library(quartose)
```

The package was originally designed to support automatic tabsets (e.g.,
writing several plots, one per tab), and to my mind this is the most
practical feature of quartose. However, pkgdown has limited support for
quarto, so it is difficult to illustrate in this document. Instead, I’ll
start by illustrating some of the simpler features. You can use
[`quarto_span()`](https://quartose.djnavarro.net/reference/quarto_object.md)
to supply CSS classes to sections of text:

``` r

quarto_span(content = "this is highlighted", class = "mark")
```

this is highlighted

If we print this object without the `results: asis` tag, you can see
what actually gets written into the document:

``` r

quarto_span(content = "this is highlighted", class = "mark")
#> [this is highlighted]{.mark}
```

On its own this isn’t entirely useful, but starts to become rather more
practical if you wrap multiple
[`quarto_span()`](https://quartose.djnavarro.net/reference/quarto_object.md)
sections within a
[`quarto_div()`](https://quartose.djnavarro.net/reference/quarto_object.md)
object.

``` r

quarto_div(
  content = list(
    quarto_span("As an illustration, you could write a paragraph that"),
    quarto_span("highlighted some sections for emphasis", class = "mark"),
    quarto_span("but left other sections unmarked."),
    quarto_span("Some things could be underlined too.", class = "underline"),
    quarto_span("You get the idea, hopefully!")
  ), 
  sep = " "
)
```

As an illustration, you could write a paragraph that highlighted some
sections for emphasisbut left other sections unmarked. *Some things
could be underlined too.* You get the idea, hopefully!

Again for illustrative purposes, we can remove the `results: asis`
option to see what’s happening under the hood:

``` r

quarto_div(
  content = list(
    quarto_span("As an illustration, you could write a paragraph that"),
    quarto_span("highlighted some sections for emphasis", class = "mark"),
    quarto_span("but left other sections unmarked."),
    quarto_span("Some things could be underlined too.", class = "underline"),
    quarto_span("You get the idea, hopefully!")
  ), 
  sep = " "
)
#> 
#> 
#> ::: {.quartose-null}
#> 
#>  [As an illustration, you could write a paragraph that]{.quartose-null} [highlighted some sections for emphasis]{.mark} [but left other sections unmarked.]{.quartose-null} [Some things could be underlined too.]{.underline} [You get the idea, hopefully!]{.quartose-null} 
#> 
#> :::
#> 
#> 
```

The same idea can be used to generate the quarto syntax to insert
section headers:

``` r

quarto_section("A level 3 heading", level = 3)
#> 
#> 
#> ### A level 3 heading
#> 
#> 
```

If the code here included the `results: asis` chunk option this would
insert a heading into the document. Similarly, there are functions like
[`quarto_markdown()`](https://quartose.djnavarro.net/reference/quarto_object.md)
that you can use to pass plain markdown:

``` r

quarto_markdown(
  content = c(
    "- this ends up being",
    "- a markdown list", 
    "- with **formatting**"
  ),
  sep = "\n"
)
```

- this ends up being
- a markdown list
- with **formatting**

Unfortunately for this vignette, the limitations of pkgdown/quarto
integration mean that some of the more fun things you can do with
quartose are hard to illustrate here. In a regular quarto project, you
could create callout boxes and margin text by defining a
[`quarto_div()`](https://quartose.djnavarro.net/reference/quarto_object.md)
with the appropriate CSS classes. If I try that here, however, it
doesn’t quite work:

``` r

quarto_div(
  content = "This would normally be a callout note.",
  class = "callout-note"
)
```

> **Note**
>
> This would normally be a callout note.

Similarly, because tabsets aren’t a supported feature in pkgdown, I
can’t easily show what a
[`quarto_tabset()`](https://quartose.djnavarro.net/reference/quarto_object.md)
looks like in this document. I can show you the quarto syntax that gets
written to the document…

``` r

cls <- unique(mpg$class)
dat <- map(cls, \(cl) filter(mpg, class == cl))
quarto_tabset(
  content = map(dat, head), 
  names = cls, 
  title = "Data", 
  level = 4
)
#> 
#> 
#> #### Data
#> 
#>  
#> 
#> 
#> ::: {.panel-tabset}
#> 
#>  
#> 
#> 
#> ##### compact
#> 
#>  
#> <pre> 
#> # A tibble: 6 × 11 
#>   manufacturer model displ  year   cyl trans      drv     cty   hwy fl    class  
#>   &lt;chr&gt;        &lt;chr&gt; &lt;dbl&gt; &lt;int&gt; &lt;int&gt; &lt;chr&gt;      &lt;chr&gt; &lt;int&gt; &lt;int&gt; &lt;chr&gt; &lt;chr&gt;  
#> 1 audi         a4      1.8  1999     4 auto(l5)   f        18    29 p     compa… 
#> 2 audi         a4      1.8  1999     4 manual(m5) f        21    29 p     compa… 
#> 3 audi         a4      2    2008     4 manual(m6) f        20    31 p     compa… 
#> 4 audi         a4      2    2008     4 auto(av)   f        21    30 p     compa… 
#> 5 audi         a4      2.8  1999     6 auto(l5)   f        16    26 p     compa… 
#> 6 audi         a4      2.8  1999     6 manual(m5) f        18    26 p     compa… 
#> </pre> 
#> 
#> 
#> ##### midsize
#> 
#>  
#> <pre> 
#> # A tibble: 6 × 11 
#>   manufacturer model      displ  year   cyl trans  drv     cty   hwy fl    class 
#>   &lt;chr&gt;        &lt;chr&gt;      &lt;dbl&gt; &lt;int&gt; &lt;int&gt; &lt;chr&gt;  &lt;chr&gt; &lt;int&gt; &lt;int&gt; &lt;chr&gt; &lt;chr&gt; 
#> 1 audi         a6 quattro   2.8  1999     6 auto(… 4        15    24 p     mids… 
#> 2 audi         a6 quattro   3.1  2008     6 auto(… 4        17    25 p     mids… 
#> 3 audi         a6 quattro   4.2  2008     8 auto(… 4        16    23 p     mids… 
#> 4 chevrolet    malibu       2.4  1999     4 auto(… f        19    27 r     mids… 
#> 5 chevrolet    malibu       2.4  2008     4 auto(… f        22    30 r     mids… 
#> 6 chevrolet    malibu       3.1  1999     6 auto(… f        18    26 r     mids… 
#> </pre> 
#> 
#> 
#> ##### suv
#> 
#>  
#> <pre> 
#> # A tibble: 6 × 11 
#>   manufacturer model       displ  year   cyl trans drv     cty   hwy fl    class 
#>   &lt;chr&gt;        &lt;chr&gt;       &lt;dbl&gt; &lt;int&gt; &lt;int&gt; &lt;chr&gt; &lt;chr&gt; &lt;int&gt; &lt;int&gt; &lt;chr&gt; &lt;chr&gt; 
#> 1 chevrolet    c1500 subu…   5.3  2008     8 auto… r        14    20 r     suv   
#> 2 chevrolet    c1500 subu…   5.3  2008     8 auto… r        11    15 e     suv   
#> 3 chevrolet    c1500 subu…   5.3  2008     8 auto… r        14    20 r     suv   
#> 4 chevrolet    c1500 subu…   5.7  1999     8 auto… r        13    17 r     suv   
#> 5 chevrolet    c1500 subu…   6    2008     8 auto… r        12    17 r     suv   
#> 6 chevrolet    k1500 taho…   5.3  2008     8 auto… 4        14    19 r     suv   
#> </pre> 
#> 
#> 
#> ##### 2seater
#> 
#>  
#> <pre> 
#> # A tibble: 5 × 11 
#>   manufacturer model    displ  year   cyl trans    drv     cty   hwy fl    class 
#>   &lt;chr&gt;        &lt;chr&gt;    &lt;dbl&gt; &lt;int&gt; &lt;int&gt; &lt;chr&gt;    &lt;chr&gt; &lt;int&gt; &lt;int&gt; &lt;chr&gt; &lt;chr&gt; 
#> 1 chevrolet    corvette   5.7  1999     8 manual(… r        16    26 p     2sea… 
#> 2 chevrolet    corvette   5.7  1999     8 auto(l4) r        15    23 p     2sea… 
#> 3 chevrolet    corvette   6.2  2008     8 manual(… r        16    26 p     2sea… 
#> 4 chevrolet    corvette   6.2  2008     8 auto(s6) r        15    25 p     2sea… 
#> 5 chevrolet    corvette   7    2008     8 manual(… r        15    24 p     2sea… 
#> </pre> 
#> 
#> 
#> ##### minivan
#> 
#>  
#> <pre> 
#> # A tibble: 6 × 11 
#>   manufacturer model       displ  year   cyl trans drv     cty   hwy fl    class 
#>   &lt;chr&gt;        &lt;chr&gt;       &lt;dbl&gt; &lt;int&gt; &lt;int&gt; &lt;chr&gt; &lt;chr&gt; &lt;int&gt; &lt;int&gt; &lt;chr&gt; &lt;chr&gt; 
#> 1 dodge        caravan 2wd   2.4  1999     4 auto… f        18    24 r     mini… 
#> 2 dodge        caravan 2wd   3    1999     6 auto… f        17    24 r     mini… 
#> 3 dodge        caravan 2wd   3.3  1999     6 auto… f        16    22 r     mini… 
#> 4 dodge        caravan 2wd   3.3  1999     6 auto… f        16    22 r     mini… 
#> 5 dodge        caravan 2wd   3.3  2008     6 auto… f        17    24 r     mini… 
#> 6 dodge        caravan 2wd   3.3  2008     6 auto… f        17    24 r     mini… 
#> </pre> 
#> 
#> 
#> ##### pickup
#> 
#>  
#> <pre> 
#> # A tibble: 6 × 11 
#>   manufacturer model       displ  year   cyl trans drv     cty   hwy fl    class 
#>   &lt;chr&gt;        &lt;chr&gt;       &lt;dbl&gt; &lt;int&gt; &lt;int&gt; &lt;chr&gt; &lt;chr&gt; &lt;int&gt; &lt;int&gt; &lt;chr&gt; &lt;chr&gt; 
#> 1 dodge        dakota pic…   3.7  2008     6 manu… 4        15    19 r     pick… 
#> 2 dodge        dakota pic…   3.7  2008     6 auto… 4        14    18 r     pick… 
#> 3 dodge        dakota pic…   3.9  1999     6 auto… 4        13    17 r     pick… 
#> 4 dodge        dakota pic…   3.9  1999     6 manu… 4        14    17 r     pick… 
#> 5 dodge        dakota pic…   4.7  2008     8 auto… 4        14    19 r     pick… 
#> 6 dodge        dakota pic…   4.7  2008     8 auto… 4        14    19 r     pick… 
#> </pre> 
#> 
#> 
#> ##### subcompact
#> 
#>  
#> <pre> 
#> # A tibble: 6 × 11 
#>   manufacturer model   displ  year   cyl trans     drv     cty   hwy fl    class 
#>   &lt;chr&gt;        &lt;chr&gt;   &lt;dbl&gt; &lt;int&gt; &lt;int&gt; &lt;chr&gt;     &lt;chr&gt; &lt;int&gt; &lt;int&gt; &lt;chr&gt; &lt;chr&gt; 
#> 1 ford         mustang   3.8  1999     6 manual(m… r        18    26 r     subc… 
#> 2 ford         mustang   3.8  1999     6 auto(l4)  r        18    25 r     subc… 
#> 3 ford         mustang   4    2008     6 manual(m… r        17    26 r     subc… 
#> 4 ford         mustang   4    2008     6 auto(l5)  r        16    24 r     subc… 
#> 5 ford         mustang   4.6  1999     8 auto(l4)  r        15    21 r     subc… 
#> 6 ford         mustang   4.6  1999     8 manual(m… r        15    22 r     subc… 
#> </pre> 
#> 
#> 
#> ::: 
#> 
#> 
```

…but I’ll concede that’s not the most helpful illustration. To that end,
instead of trying to work around the limitations within pkgdown, it is
probably easier to write a blog post at
[blog.djnavarro.net](https://blog.djnavarro.net) showing how it works.

More on that to come, I suppose :)

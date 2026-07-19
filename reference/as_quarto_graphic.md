# Tag an object as a graphic for `quarto_tabset()`

[`quarto_tabset()`](https://quartose.djnavarro.net/reference/quarto_object.md)
auto-detects several kinds of graphics objects (ggplot2/ patchwork, base
R recorded plots, grid grobs, and lattice/trellis objects) and renders
them as images rather than as captured text output. If `content`
includes a graphics object from a package quartose doesn't know about,
wrap it in `as_quarto_graphic()` to force this treatment.

## Usage

``` r
as_quarto_graphic(x)
```

## Arguments

- x:

  An object to tag as a graphic.

## Value

`x`, with the additional class `"quartose_graphic"` prepended.

## Details

Objects tagged this way are rendered via a generic fallback: a PNG
device is opened and `print(x)` is called on the (untagged) object, on
the assumption that printing it draws a plot to the active graphics
device. This works for many graphics-producing S3 objects but is
best-effort; if `print(x)` doesn't draw anything, the resulting image
will be blank.

## Examples

``` r
# a hypothetical object whose print method draws a plot, but which
# quartose doesn't otherwise recognize as a graphic
obj <- structure(list(), class = "some_custom_plot_class")
tagged <- as_quarto_graphic(obj)
class(tagged)
#> [1] "quartose_graphic"       "some_custom_plot_class"
```

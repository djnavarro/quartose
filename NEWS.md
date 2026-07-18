# quartose (development version)

* Fixed a bug where `format.quarto_tabset()` did not escape `<`/`>` in
  captured output from arbitrary R objects (e.g. a tibble's `<fct>`
  column-type tag), which could be parsed as an unknown HTML tag by
  quarto/pandoc (#1).
* Fixed a bug where the internal `quarto_plot` wrapper used to capture
  ggplot2 objects inside `quarto_tabset()` had its class silently stripped
  by `c()`-based list flattening in `format.quarto_tabset()`. Plot capture
  now preserves the `quarto_plot` class and is dispatched via a dedicated
  `knit_print.quarto_plot()` method instead of relying on `ggplot`'s own
  `knit_print` method by coincidence.

# quartose 0.1.0

* Initial CRAN submission.

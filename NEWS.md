# quartose (development version)

* Fixed a bug where `format.quarto_tabset()` did not escape `<`/`>` in
  captured output from arbitrary R objects (e.g. a tibble's `<fct>`
  column-type tag), which could be parsed as an unknown HTML tag by
  quarto/pandoc (#1). `?quarto_format` now documents the resulting escaping
  policy: only `quarto_tabset()`'s captured object output is escaped this
  way; `quarto_span()`/`quarto_div()` content is validated rather than
  captured, and `quarto_markdown()` passes raw markup through untouched by
  design.
* Fixed a bug where the internal `quarto_plot` wrapper used to capture
  ggplot2 objects inside `quarto_tabset()` had its class silently stripped
  by `c()`-based list flattening in `format.quarto_tabset()`. Plot capture
  now preserves the `quarto_plot` class and is dispatched via a dedicated
  `knit_print.quarto_plot()` method instead of relying on `ggplot`'s own
  `knit_print` method by coincidence.
* `quarto_div()` now validates its `content` argument: every element must be
  a character vector or a `quarto_object`, mirroring the checks already
  applied by `quarto_group()`/`quarto_markdown()`. Previously an unsupported
  object (e.g. a bare list, a model object, or a number) would pass silently
  into `quarto_div()` and only fail later inside `format()`. An empty div
  (`content = NULL`, `content = list()`, or `content = character(0)`) is
  still permitted.
* `quarto_tabset()` now raises a specific error ("content has no names, and
  `names` was not supplied...") when `content` is unnamed and `names` isn't
  supplied, instead of the generic (and misleading) "names must be a
  character vector".
* `quarto_tabset()` now auto-detects and renders a wider range of graphics
  objects as images, rather than only ggplot2 (#2): base R recorded plots
  (`grDevices::recordPlot()`), grid grobs, lattice/trellis objects, and
  patchwork objects (which subclass ggplot2 and already worked once the
  `quarto_plot` flattening bug was fixed). Added `as_quarto_graphic()`, an
  escape hatch that tags an arbitrary object so it is treated as a graphic
  even when quartose doesn't otherwise recognize its class. This adds
  `grDevices` and `grid` to `Imports`; both are base R packages bundled
  with every R installation, so this does not add any new installation
  burden.
* `quarto_div()` now supports graphics objects in `content` (#3), using the
  same detection and rendering machinery as `quarto_tabset()`. `format()`
  returns a single string as before when `content` has no graphics
  objects, and a list (mixing strings and plot objects, as `quarto_tabset()`
  already does) when it does.

# quartose 0.1.0

* Initial CRAN submission.

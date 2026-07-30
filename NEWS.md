# quartose (development version)

* `quarto_div()` now accepts objects like `knitr::kable(format = "html")`,
  `flextable`, and `gt` tables as `content` -- previously most of these were
  either mis-rendered (e.g. `flextable`, coerced through `format()`) or
  rejected outright, depending on the object. `check_args_div()` detects
  these via the same `"knit_asis"`-output check used by `quarto_tabset()`
  (see below), and `format.quarto_div()` extracts and emits their raw
  markup unescaped, consistent with how `quarto_tabset()` now handles the
  same kinds of objects. Objects that neither print like ordinary R
  objects nor return `"knit_asis"` output (a bare list, a model object, a
  number, etc.) are still rejected at construction time with the existing
  informative error.
* Fixed a bug where `format.quarto_tabset()` mishandled content whose
  `knit_print()` method returns output marked via `knitr::asis_output()`
  (class `"knit_asis"`) rather than printing it as a side effect — this
  includes `knitr::kable(format = "html")`, `flextable` objects, and most
  htmlwidget-like objects. Previously such content was displayed as the
  literal, quoted R representation of the return value (including its
  `attr(,"class")` dump) inside a `<pre>` block, rather than as rendered
  markup. `format.quarto_tabset()` now detects `"knit_asis"` output and
  emits it as raw, unescaped markup so it renders as intended. `?quarto_format`'s
  escaping-policy documentation is updated accordingly.

# quartose 0.2.0

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

* Initial CRAN submission. This release establishes the core design that
  later versions build on: constructors validate and store their arguments
  unprocessed (`R/class.R`), `format.<class>()` methods turn a stored object
  into Quarto syntax on demand (`R/format.R`), and a shared
  `print.quarto_object()`/`knit_print.quarto_object()` pair (`R/print.R`)
  gives a human-readable console summary via `print()` and emits the actual
  Quarto markup via `cat()` when `knitr::knit_print()` is called inside a
  `results: asis` chunk.
* Provides six constructors, all returning an object with parent S3 class
  `quarto_object` plus a class-specific subclass: `quarto_section()` for
  headers; `quarto_tabset()` for tabsets, with optional section title and
  manual tab naming via `names`; `quarto_div()` and `quarto_span()` for
  divs and spans with CSS classes; `quarto_markdown()` for passing raw
  markdown/character content through untouched; and `quarto_group()` for
  bundling several quarto objects so they print together.
* `quarto_tabset()` content can include ggplot2 objects: these are captured
  and rendered only when `knit_print()` is called, via an internal
  `quarto_plot` wrapper. `quarto_div()` does not yet support plot content.
* Basic argument validation (`R/validate.R`) checks types and lengths for
  each constructor (e.g. `level` must be a whole number between 1 and 6;
  `quarto_group()`/`quarto_markdown()` require their `content` elements to
  all be quarto objects or character vectors, respectively), raising
  informative errors via `rlang::abort()`.
* Imports: cli, knitr, purrr, rlang, utils. Suggests: ggplot2, quarto,
  rmarkdown, spelling, testthat (>= 3.0.0).

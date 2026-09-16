# AGENTS.md

## Package overview

**quartose** is an R package for dynamically generating Quarto syntax
from within R code chunks (e.g. section headers, tabsets, divs, spans,
callouts, raw markdown). Rather than hand-writing Quarto markup, users
construct S3 objects that are printed as Quarto syntax when knit, so
document elements can be generated programmatically.

- Repo: <https://github.com/djnavarro/quartose>
- Docs site: <https://quartose.djnavarro.net/>
- Lifecycle: experimental
- License: MIT

## Architecture

All exported constructors return an S3 object of class `quarto_object`
(plus a specific subclass). The design deliberately separates three
concerns:

1.  **Construction** (`R/class.R`) — validate inputs and store them
    unprocessed.
2.  **Formatting** (`R/format.R`) — `format.<class>()` methods that
    convert a stored object into Quarto syntax text on demand.
3.  **Printing** (`R/print.R`) —
    [`print()`](https://rdrr.io/r/base/print.html) gives a
    human-readable summary in the R console;
    [`knitr::knit_print()`](https://rdrr.io/pkg/knitr/man/knit_print.html)
    emits the actual Quarto syntax (via
    [`cat()`](https://rdrr.io/r/base/cat.html), bypassing normal knitr
    formatting) so it is parsed correctly by Quarto. This requires the
    chunk option `results: asis`.

Exported constructors:
[`quarto_section()`](https://quartose.djnavarro.net/reference/quarto_object.md),
[`quarto_tabset()`](https://quartose.djnavarro.net/reference/quarto_object.md),
[`quarto_div()`](https://quartose.djnavarro.net/reference/quarto_object.md),
[`quarto_span()`](https://quartose.djnavarro.net/reference/quarto_object.md),
[`quarto_markdown()`](https://quartose.djnavarro.net/reference/quarto_object.md),
[`quarto_group()`](https://quartose.djnavarro.net/reference/quarto_object.md).

Input validation lives in `R/validate.R` and is shared across
constructors.

When adding a new quarto element type, follow this same three-part
pattern (constructor + validator in `class.R`/`validate.R`,
`format.<class>()` in `format.R`, and rely on the shared
`print.quarto_object`/`knit_print.quarto_object` machinery in `print.R`
unless it needs bespoke behavior).

## Development workflow

This is a standard devtools/testthat/roxygen2 package.

``` r

devtools::load_all()     # load package for interactive testing
devtools::document()     # regenerate NAMESPACE/man/ from roxygen comments
devtools::test()         # run testthat suite
devtools::check()        # full R CMD check
```

- Roxygen2 (markdown enabled) is the source of truth for `NAMESPACE` and
  `man/*.Rd` — always edit roxygen comments in `R/`, never `NAMESPACE`
  or `man/` directly.
- Testing uses testthat edition 3. Test files live in `tests/testthat/`
  and mirror the source files (`test-class.R`, `test-format.R`,
  `test-print.R`, `test-validate.R`). Add tests alongside new/changed
  behavior in the matching file.
- Keep dependencies minimal. Current `Imports`: cli, knitr, purrr,
  rlang, utils. `ggplot2`, `quarto`, `rmarkdown`, `spelling` are
  `Suggests` only — don’t add new hard dependencies without good reason.
- `README.md` is generated from `README.Rmd`; edit the `.Rmd` and
  re-knit rather than editing `README.md` directly.
- `NEWS.md` should be updated for user-facing changes.

## Known issues & roadmap

See [PLAN.md](https://quartose.djnavarro.net/PLAN.md) for the current
triage of open GitHub issues, known weaknesses found in code review
(e.g. HTML-escaping gaps, the `quarto_plot`/tabset graphics-capture
handling), and the phased plan for addressing them. Consult it before
starting work in `format.R`’s graphics-capture path or the validation
logic in `validate.R`, and update it as issues are resolved.

## Conventions

- Use base R pipe `|>`, not magrittr `%>%`.
- Follow existing validation style in `validate.R` (informative errors
  via `cli` /
  [`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html)), and
  reuse existing validators where possible rather than writing new ad
  hoc checks.
- Plot objects (ggplot2) inside tabsets are captured and rendered
  specially during printing — be careful not to break this path when
  touching `print.R` or `format.R`.

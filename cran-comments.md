## Summary

This is an update to `quartose`, currently on CRAN as version 0.2.0. This is
a patch release (0.2.1) with a single, related pair of bug fixes: a
`"knit_asis"`-output handling bug in `format.quarto_tabset()` (content
whose `knit_print()` method returns output marked via
`knitr::asis_output()` -- e.g. `knitr::kable(format = "html")`, `flextable`
objects, and most htmlwidget-like objects -- was previously displayed as
the literal, quoted R representation of the return value rather than
rendered markup), and the same fix extended to `quarto_div()`, which now
also accepts these object types as `content` rather than mis-rendering or
rejecting them. Both bugs affected real downstream usage. See `NEWS.md`
for the full entry.

## R CMD check results (local)

0 errors | 0 warnings | 0 notes

## URL check

`urlchecker::url_check()`: all URLs correct, no changes.

## Rhub platforms tested

`quartose` contains no compiled code of its own, so this submission targets
a curated set of Rhub platforms relevant to the package's actual risk
surface, rather than the full compiler/sanitizer matrix (which mostly
re-exercises R code under toolchains that never touch anything compiled):

https://github.com/djnavarro/quartose/actions/runs/33822633887

- `macos`, `macos-arm64`: OS/architecture coverage not otherwise provided
  by win-builder (Windows) or local checks (Linux).
- `nosuggests`: exercises the package with all `Suggests` absent, matching
  quartose's `requireNamespace()`-guarded optional support for `ggplot2`,
  `flextable`, `dplyr`, `lattice`, `patchwork`, `quarto`, and `rmarkdown`.
- `donttest`: runs `\donttest` examples.
- `vnu`: validates generated HTML against the HTML5 spec, relevant given
  that quartose's purpose is emitting raw markup into rendered documents.

All 5 platforms pass with no errors, warnings, or notes.

## Win-builder platforms tested

- `devtools::check_win_release()`: https://win-builder.r-project.org/65Tzd6VxzCh6/00check.log — Status: OK
- `devtools::check_win_devel()`: https://win-builder.r-project.org/K47yGve2p5u4/00check.log — Status: OK

Both checks are clean: 0 errors, 0 warnings, 0 notes.

## Downstream dependencies

`quartose` has no reverse dependencies on CRAN.

Kind regards
Danielle Navarro

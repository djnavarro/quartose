## Summary

This is an update to `quartose`, currently on CRAN as version 0.1.0. This
release (0.2.0) fixes two bugs found via user reports (an HTML-escaping gap
in `quarto_tabset()`, and a `quarto_plot` class-stripping bug that broke
plot capture in tabsets), tightens input validation for `quarto_div()` and
`quarto_tabset()`, and adds modest new functionality: `quarto_tabset()` now
auto-detects a wider range of graphics objects (base R recorded plots, grid
grobs, lattice/trellis, patchwork), a new `as_quarto_graphic()` escape hatch
lets users tag arbitrary objects as graphics, and `quarto_div()` now
supports graphics content using the same machinery. See `NEWS.md` for the
full list of changes.

## R CMD check results (local)

0 errors | 0 warnings | 0 notes

Running `R CMD check --as-cran` directly (rather than through
`devtools::check()`) surfaces one additional informational note, which is a
pre-existing, known artifact of the `spelling` package's `tests/spelling.R`
template (a `Rout`/`Rout.save` whitespace/echo-style comparison mismatch,
not a real failure; the test itself uses `error = FALSE`). This is
unrelated to any change in this release.

## Rhub platforms tested

Checked on the same Rhub platform set used for the 0.1.0 submission (30
platforms):

https://github.com/djnavarro/quartose/actions/runs/30180624311

Passes on 20/30 with no errors or warnings. There are two categories of
failure, both unrelated to quartose (which contains no compiled code of its
own):

- `clang16`, `clang17`, `clang18`, `clang19`, `clang20`, `c23`, `gcc15`,
  `noremap` (8 platforms): all fail identically with
  `Error: .onLoad failed in loadNamespace() for 'vctrs', details: error:
  symbol bindings not supported yet`. This is a binary-compatibility issue
  between the CRAN binary of the `vctrs` package (a transitive dependency
  via `dplyr`/`ggplot2`, used only in `Suggests`) and these
  bleeding-edge/experimental compiler toolchains provided by Rhub — it
  occurs before any quartose code runs, purely while loading `vctrs`.
- `valgrind` (1 platform): `R CMD check` itself reports `Status: OK`
  (0 errors, 0 warnings, 0 notes, all tests passing); the job is marked
  failed only because Valgrind's memcheck flags "definitely lost" bytes.
  Every leak's stack trace traces into system font-rendering libraries
  (`libfontconfig`, `libpango`, `libcairo`) invoked by R's graphics engine
  when a PNG device is opened to render captured plots — a well-known
  category of Valgrind false positive for any package that opens a
  graphics device on this image, and does not involve any quartose code.

Happy to investigate further if you believe either is relevant to this
submission.

## Win-builder platforms tested

- `devtools::check_win_devel()`: https://win-builder.r-project.org/ (submitted; results emailed)
- `devtools::check_win_release()`: https://win-builder.r-project.org/ (submitted; results emailed)

R CMD check logs look okay locally; win-builder logs to be confirmed once emailed.

## Downstream dependencies

`quartose` has no reverse dependencies on CRAN.

Kind regards
Danielle Navarro

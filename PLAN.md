# PLAN.md

Living document tracking known issues, weaknesses found during review,
and the plan for addressing them. Update this file (and `NEWS.md`) as
items are resolved; don’t let it silently go stale.

Last reviewed: 2026-07-19.

## Open items

None currently tracked.

## Housekeeping notes

- The `@details` section of
  [`?quarto_format`](https://quartose.djnavarro.net/reference/quarto_format.md)
  (`R/format.R`) now documents the escaping policy: only
  [`quarto_tabset()`](https://quartose.djnavarro.net/reference/quarto_object.md)’s
  captured object output is escaped (`<`/`>` → `&lt;`/`&gt;`);
  [`quarto_span()`](https://quartose.djnavarro.net/reference/quarto_object.md)/[`quarto_div()`](https://quartose.djnavarro.net/reference/quarto_object.md)
  content is restricted by validation to character/quarto-object/graphic
  (never arbitrary captured output);
  [`quarto_markdown()`](https://quartose.djnavarro.net/reference/quarto_object.md)
  is untouched by design.
- `Imports` grew only as expected for graphics support (`grDevices`,
  `grid` — both base R, no new installation burden); no other new hard
  dependencies were added.
- Upgraded to roxygen2 8.0.0 (`RoxygenNote` in `DESCRIPTION`), which
  requires `@aliases` to be a single line. Reflowed the three multi-line
  `@aliases` blocks (`class.R`, `format.R`, `print.R`) into single
  lines; regenerated `NAMESPACE`/`man/*.Rd` and confirmed `NAMESPACE`
  and all `.Rd` files other than `man/quarto_format.Rd` (which picked up
  the new escaping-policy paragraph) are byte-for-byte unchanged.
  `devtools::check()` now runs clean modulo one pre-existing NOTE
  (`AGENTS.md`/`PLAN.md` being non-standard top-level files, expected
  for this project) and the spelling test’s informational NOTE (not a
  failure; `error = FALSE`). Full `testthat` suite passes (all files,
  including the integration tests).
- Added `pandoc` and `tibble's` to `inst/WORDLIST` (introduced by the
  new escaping-policy doc paragraph). The remaining pre-existing
  spelling-test hits (`qreport`, `quartabs`, `Sasaki`, `Yusuke` — proper
  nouns/package names in the “Related work” section) are now in
  `inst/WORDLIST` too, and the stray `'s` hit from `print()'s` (a
  markdown code-span tokenization artifact, not a real word) was fixed
  by rewording the README sentence. `spelling::spell_check_package(".")`
  now reports no spelling errors.
- `AGENTS.md`/`PLAN.md` added to `.Rbuildignore`, clearing the
  “Non-standard files/directories found at top level” NOTE.
  `devtools::check()` now passes with 0 errors, 0 warnings, 0 notes.

## Resolved issue & finding log

All four tracked GitHub issues are closed, and all code-review findings
uncovered along the way (including the escaping-policy documentation
gap) are resolved.

| Item | Resolution |
|----|----|
| [\#1](https://github.com/djnavarro/quartose/issues/1) — protect `<`/`>` in HTML output | `protect_angle_brackets()` (`R/format.R`) escapes captured output in [`format.quarto_tabset()`](https://quartose.djnavarro.net/reference/quarto_format.md). Tests in `test-format.R`; documented in `NEWS.md`. |
| [\#2](https://github.com/djnavarro/quartose/issues/2) — wider range of graphics objects | `is_graphic()` (`R/validate.R`) generalizes detection to ggplot2/patchwork, base R recorded plots, grid grobs, and lattice/trellis objects, plus the [`as_quarto_graphic()`](https://quartose.djnavarro.net/reference/as_quarto_graphic.md) tagging escape hatch. Rendered via `render_graphic_png()` + `knit_print.quarto_plot()`. |
| [\#3](https://github.com/djnavarro/quartose/issues/3) — graphics within divs | `check_args_div()`/[`format.quarto_div()`](https://quartose.djnavarro.net/reference/quarto_format.md) reuse the `is_graphic()`/`quarto_plot` path added for \#2. |
| [\#4](https://github.com/djnavarro/quartose/issues/4) — quartose and revealjs | Diagnosed as not a quartose bug: [`print()`](https://rdrr.io/r/base/print.html)’s console summary is routed through [`message()`](https://rdrr.io/r/base/message.html) by `cli` off an interactive terminal, and revealjs suppresses `message`/`warning` chunk output by default; the actual `knit_print()` + `results: asis` document-generation path renders correctly under revealjs. Doc clarification added to [`?quarto_print`](https://quartose.djnavarro.net/reference/quarto_print.md) and the README. Issue closed. |
| `quarto_plot()` class silently stripped by [`c()`](https://rdrr.io/r/base/c.html)-flattening | Fixed by wrapping appended elements in `list(...)` in [`format.quarto_tabset()`](https://quartose.djnavarro.net/reference/quarto_format.md)/[`format.quarto_div()`](https://quartose.djnavarro.net/reference/quarto_format.md); added a real `knit_print.quarto_plot()` method so dispatch is intentional rather than coincidental (previously relied on `knitr`’s native `knit_print.ggplot` by chance). Test asserts the plot element’s *class* in [`format()`](https://rdrr.io/r/base/format.html) output. |
| [`quarto_div()`](https://quartose.djnavarro.net/reference/quarto_object.md) content unvalidated | `check_args_div()` now requires every content element to be character, `quarto_object`, or a recognized graphic, mirroring `check_args_group()`/`check_args_markdown()`. Empty divs remain a supported edge case. Tests in `test-validate.R`. |
| Unhelpful error for unnamed tabset content | `check_args_tabset()` now raises a specific message (“content has no names, and `names` was not supplied…”) instead of the generic “names must be a character vector” type error, confirmed with the maintainer as the desired behavior (no auto-generated default labels). Tests in `test-validate.R`. |
| No end-to-end rendering test | `tests/testthat/test-integration.R` renders a minimal `.qmd` per constructor (all six) via [`quarto::quarto_render()`](https://quarto-dev.github.io/quarto-r/reference/quarto_render.html) and asserts on the resulting HTML. Skips cleanly when `quarto`/`rmarkdown` or the Quarto CLI aren’t available. Verified against Quarto CLI 1.5.52. |
| Escaping policy undocumented | [`?quarto_format`](https://quartose.djnavarro.net/reference/quarto_format.md)’s `@details` (`R/format.R`) now spells out that only [`quarto_tabset()`](https://quartose.djnavarro.net/reference/quarto_object.md)’s captured object output is escaped; [`quarto_span()`](https://quartose.djnavarro.net/reference/quarto_object.md)/[`quarto_div()`](https://quartose.djnavarro.net/reference/quarto_object.md) content is validated rather than captured, and [`quarto_markdown()`](https://quartose.djnavarro.net/reference/quarto_object.md) is untouched by design. |

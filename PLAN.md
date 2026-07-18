# PLAN.md

Living document tracking known issues, weaknesses found during review, and the
plan for addressing them. Update this file (and `NEWS.md`) as items are
resolved; don't let it silently go stale.

Last reviewed: 2026-07-19.

## Open GitHub issues

| # | Title | Type | Notes |
|---|-------|------|-------|
| [#1](https://github.com/djnavarro/quartose/issues/1) | protect "<" and ">" characters in HTML output | bug | Confirmed reproducible (see below). Highest priority — silent output corruption. |
| [#2](https://github.com/djnavarro/quartose/issues/2) | support wider range of graphics objects | enhancement | RESOLVED for `quarto_tabset()` — see remediation step 5 below. `is_graphic()` now covers ggplot2/patchwork, recorded plots, grobs, and trellis objects, plus the `as_quarto_graphic()` tagging escape hatch. Divs still don't support graphics at all (#3) — that's a separate, still-open follow-up (step 6). |
| [#3](https://github.com/djnavarro/quartose/issues/3) | allow graphics objects within divs | enhancement | `format.quarto_div()` always coerces content via `paste()`/`unlist()`; no plot-capture path exists for divs at all (only tabsets have one). |
| [#4](https://github.com/djnavarro/quartose/issues/4) | quartose and revealjs format | not a quartose bug (diagnosed) | Reproduced and root-caused — see below. No code fix needed in `format.R`/`print.R`; the correct usage pattern already works under revealjs. A doc clarification is worth making. |

There are no open pull requests.

## Issue #4 diagnosis (revealjs)

Reproduced locally with `quarto` 1.5.52 using the reporter's exact example
(`quarto_tabset()` + `print(tabs)`, `format: revealjs`, no `results: asis`).
Three follow-up experiments isolate the cause:

1. **The reporter's exact repro** — a chunk that builds a `quarto_tabset()`
   and calls `print(tabs)` (not `knitr::knit_print()`), with no
   `results: asis` — renders a completely empty slide under `revealjs`: no
   `cell`/`cell-output` div at all makes it into the HTML.
2. Rendering the *same* chunk to plain `format: html` **does** show output —
   a `cell-output-stderr` block containing the `print.quarto_object()`
   summary (`<quarto_tabset>`, `• content: <list>`, ...). This shows the
   content isn't failing to evaluate; it's specifically absent under
   revealjs.
3. Isolating further: a chunk with `cat()`/`print()` (stdout) and
   `message()`/`warning()` (stderr) shows the stdout lines under revealjs's
   defaults, but the stderr lines are missing entirely — until they're
   force-enabled with `#| message: true` / `#| warning: true`, at which
   point they render identically to `html`. So **revealjs's default execute
   options suppress message/warning-stream chunk output**, while `html`'s
   defaults show it.
4. `print.quarto_object()` builds its console summary with
   `cli::cli_text()`/`cli::cli_ul()`. `cli` routes output through
   `message()` (not `cat()`/stdout) when not attached to an interactive
   terminal — which is exactly the case during a `quarto render`. That's why
   the reporter's `print(tabs)` call vanished under revealjs but was visible
   under `html`.
5. Separately, and more importantly: the actual document-generation path
   (`quarto_tabset()` + `knitr::knit_print()` inside a `results: asis`
   chunk) was tested directly under `revealjs` and rendered correctly —
   `panel-tabset` markup, section title, and tab names all present in the
   output HTML.

**Conclusion:** this is not a bug in quartose's syntax generation. It's a
combination of (a) the reporter using `print()`, which by design only
produces a console summary and was never intended to write to the document
(see `?quarto_print`), and (b) that console summary happening to be
invisible on revealjs specifically because of `cli`'s message-stream routing
combined with revealjs's default `warning`/`message` suppression — a
Quarto/`cli` interaction unrelated to quartose's own code. No `format.R`/
`print.R` fix is planned for this. Suggested follow-ups (not yet done,
pending maintainer input):

- Reply on #4 explaining the diagnosis above and pointing to
  `knitr::knit_print()` + `results: asis` as the correct pattern.
- Consider a small doc clarification in `?quarto_print`/README noting that
  `print()` output is console-only and, on some renderers (e.g. revealjs),
  may not appear at all if debugging via `print()` mid-render — use
  `knitr::knit_print()` for the actual document output.

## Additional weaknesses found during code review

### 1. `quarto_plot()` wrapper is effectively dead code (related to #2/#3)

In `format.quarto_tabset()` (`R/format.R:126-128`), a captured ggplot is
appended with:

```r
out <- c(out, quarto_plot(content = x$content[[i]]))
```

Because `quarto_plot()` returns a list (`quarto_object`s are lists under the
hood) and `out` is a plain list, `c()` **flattens** it instead of appending it
as a single element. I confirmed this directly:

```r
tt <- quarto_tabset(content = list(a = ggplot2::ggplot()), level = 2L)
fmt <- format(tt)
class(fmt[[4]])
#> [1] "gg" "ggplot"        # NOT "quarto_plot"/"quarto_object" — class was stripped
```

The `quarto_plot` class has no `format.quarto_plot()` or `knit_print.quarto_plot()`
method anywhere in the package, so it currently only "works" because the
flattened list happens to contain a bare `ggplot` object, and `knitr` already
has its own `knit_print.ggplot` method. This is fragile: it depends on an
accidental interaction between list-flattening and `is.character()`/`else`
branching in `knit_print.quarto_object()` (`R/print.R:106-113`), not on any
deliberate contract. Any refactor of the graphics-capture path (needed for
#2/#3 anyway) should replace this with an explicit, tested mechanism — e.g.
`out[[length(out) + 1]] <- quarto_plot(...)` (or build `out` with
`purrr::list_flatten()`-safe append), plus real `format.quarto_plot()`/
`knit_print.quarto_plot()` methods so the dispatch is intentional rather than
coincidental.

### 2. `quarto_div()` content is essentially unvalidated — RESOLVED

~~`check_args_div()` (`R/validate.R:23-31`) takes a `content` argument but
never uses it~~ — `check_args_div()` now checks that every element of
`content` is either a character vector or a `quarto_object`, mirroring
`check_args_group()`/`check_args_markdown()`. An empty div (`content = NULL`,
`list()`, or `character(0)`) remains a supported edge case (see the existing
"divs can be pretty flexible" test intent) and is special-cased rather than
rejected. Tests added to `test-validate.R` covering both the newly-rejected
element types (numbers, logicals, nested lists, data frames, model objects)
and the still-valid empty/character/quarto-object cases.

### 3. Unhelpful error when tabset content is unnamed and `names` is omitted — RESOLVED

Per the docs, `names = NULL` should fall back to `names(content)`. If
`content` is an unnamed list and `names` isn't supplied, `names(content)` is
`NULL`, and `check_args_tabset()` used to abort with the generic message
`"names must be a character vector"` — which didn't tell the user *why*
(missing names on content, not a type error on an explicit `names` argument).

Confirmed with the maintainer: keep this a hard error (don't auto-generate
default labels like `"Tab 1"`, `"Tab 2"`), but make the message specific.
`check_args_tabset()` now detects the case where `names` is still `NULL` at
validation time — which, given `quarto_tabset()`'s fallback logic, can only
happen when the caller didn't supply `names` *and* `content` has no names of
its own — and raises `"content has no names, and \`names\` was not
supplied..."` instead of the generic type error. A genuinely wrong-typed
`names` (e.g. a number or list) still gets the original `"names must be a
character vector"` message. Tests added to `test-validate.R`.

### 4. Escaping is one instance of a broader "raw text vs. markup" ambiguity

Issue #1 is scoped to `<`/`>`, but the same ambiguity applies more broadly:
`quarto_span()`/`quarto_div()` content and tabset content run through
`capture.output(knitr::knit_print(...))` or direct `paste()`, with no
consistent policy on which content is literal text needing escaping vs.
markdown/HTML the user intentionally wants passed through raw (e.g.
`quarto_markdown()` is explicitly meant to carry raw markup). A fix for #1
should define this policy explicitly (likely: escape by default for
`quarto_span`/`quarto_div`/tabset content coming from arbitrary R objects;
leave `quarto_markdown()` untouched) rather than patching only the literal
`<fct>` case from the issue.

## Remediation plan

### Phase 1 — correctness fixes (target: 0.1.1 patch release)

1. **Fix #1.** Add a small escaping helper (e.g. `escape_html_brackets()`
   in `validate.R` or a new `utils.R`) and apply it wherever arbitrary R
   object output is captured (`format.quarto_tabset()`'s
   `capture.output(knit_print(...))` path in particular). Add regression
   tests in `test-format.R` using factor/tibble output like the original
   report. Document the escaping policy from finding #4 above in
   `?quarto_format`.
2. **Fix the `quarto_plot` flattening bug** (finding #1). Replace the `c()`
   append with an explicit single-element insert, and add real
   `format.quarto_plot()`/`knit_print.quarto_plot()` methods so dispatch is
   intentional. Add a test asserting the *class* of the plot element in
   `format()` output, not just that it's a ggplot.
3. **Tighten `quarto_div()` validation** (finding #2): content elements must
   be character or `quarto_object`, mirroring `check_args_markdown()`/
   `check_args_group()`. Add tests for the new error.
4. Improve the tabset unnamed-content error message (finding #3); confirm
   desired behavior with the maintainer first (friendlier error vs. default
   labels) since it changes user-facing behavior either way.

### Phase 2 — graphics support (target: 0.2.0)

5. **#2 — wider graphics support — RESOLVED for `quarto_tabset()`.**
   `is_graphic()` (`R/validate.R`) generalizes `is_ggplot()` to a small
   dispatch table covering ggplot2 (and patchwork, which subclasses
   `ggplot` and already worked once the flattening bug was fixed — verified
   with a test), base R recorded plots (`grDevices::recordPlot()`), grid
   grobs, and lattice/trellis objects. `format.quarto_tabset()` now checks
   `is_graphic()` instead of `is_ggplot()`. Rendering for the non-ggplot
   classes is handled by a new `render_graphic_png()` helper in
   `knit_print.quarto_plot()`: it opens a PNG device, draws the object
   (`replayPlot()` for recorded plots, `grid::grid.draw()` for grobs,
   `print()` for trellis/tagged objects), and embeds the result via
   `knitr::include_graphics()`. ggplot/patchwork content still goes through
   knitr's own native `knit_print.ggplot` rendering rather than this PNG
   fallback. This adds `grDevices` and `grid` to `Imports` — both are base
   R packages bundled with every R installation, so no new installation
   burden. Added the user-tagging escape hatch requested in the issue:
   `as_quarto_graphic()`, a helper users can wrap objects in when
   auto-detection fails.
6. **#3 — graphics in divs.** Once the tabset graphics-capture path is
   generalized (step 5), reuse the same helper in `format.quarto_div()` so
   divs can contain plots the way tabsets can. Update `class.R` docs, which
   currently state plots in divs are unsupported.

### Phase 3 — investigation (unscheduled until reproduced)

7. **#4 — revealjs.** Reproduce the reporter's minimal example locally
   (`quarto::quarto_render()` on a small `format: revealjs` document, already
   available via the `quarto` Suggests dependency) and determine root cause:
   quartose-specific bug, a Quarto/pandoc revealjs limitation with `asis`
   chunks, or a usage issue. Document the finding on the issue before
   deciding on a fix vs. a documentation note about revealjs limitations.

### Testing improvements alongside the above

- Add a guarded integration test (skip if `quarto::quarto_available()` is
  `FALSE` or pandoc isn't present) that actually renders a minimal `.qmd`
  containing each `quarto_*()` constructor, to catch rendering-level
  regressions (like #4) that unit tests on `format()`/`knit_print()` output
  alone can't catch.
- Each fix above should land with tests in the matching `test-*.R` file per
  the convention in `AGENTS.md`.

### Housekeeping

- Update `NEWS.md` for every user-facing fix.
- Re-run `devtools::check()` before release; current `Imports` should remain
  unchanged unless graphics-support work (Phase 2) requires a genuinely new
  hard dependency (avoid if possible — keep it in `Suggests` with graceful
  fallback, following existing `ggplot2` precedent).

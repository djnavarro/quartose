# end-to-end integration tests: rendering via quarto ------------------------
#
# The tests in test-format.R / test-print.R exercise format() and
# knit_print() output directly, but never actually invoke the Quarto CLI.
# That leaves a gap: rendering-level regressions (like issue #4, where a
# `results: asis` chunk behaved differently under revealjs) can't be caught
# by those unit tests alone. These tests render a minimal .qmd for each
# constructor via quarto::quarto_render() and inspect the resulting HTML.
#
# Skipped whenever the `quarto` package or the Quarto CLI itself isn't
# available (e.g. CRAN, many CI runners), so they never block a build that
# lacks Quarto.

skip_if_no_quarto <- function() {
  testthat::skip_if_not_installed("quarto")
  testthat::skip_if_not_installed("rmarkdown")
  if (!isTRUE(quarto::quarto_available())) {
    testthat::skip("Quarto CLI is not available")
  }
}

# Renders `chunk_code` (a string of R source, evaluated inside a single
# results:asis chunk with quartose already attached) inside a minimal .qmd
# and returns the rendered HTML as a single string.
render_quarto_chunk <- function(chunk_code) {
  qmd <- tempfile(fileext = ".qmd")
  html <- sub("\\.qmd$", ".html", qmd)
  on.exit(unlink(c(qmd, html)), add = TRUE)

  writeLines(
    c(
      "---",
      "title: quartose integration test",
      "format: html",
      "---",
      "",
      "```{r}",
      "#| echo: false",
      "#| results: asis",
      "library(quartose)",
      chunk_code,
      "```"
    ),
    qmd
  )

  quarto::quarto_render(qmd, output_format = "html", quiet = TRUE)
  paste(readLines(html, warn = FALSE), collapse = "\n")
}

test_that("quarto_section renders a heading via quarto render", {
  skip_if_no_quarto()
  out <- render_quarto_chunk(
    'knitr::knit_print(quarto_section("Quartose Integration Heading", level = 2L))'
  )
  expect_match(out, "Quartose Integration Heading", fixed = TRUE)
  expect_match(out, "<h2[^>]*>Quartose Integration Heading</h2>", perl = TRUE)
})

test_that("quarto_tabset renders tab content via quarto render", {
  skip_if_no_quarto()
  out <- render_quarto_chunk(
    paste(
      "tt <- quarto_tabset(",
      '  content = list(alpha = "alpha tab content", beta = "beta tab content"),',
      "  level = 2L",
      ")",
      "knitr::knit_print(tt)",
      sep = "\n"
    )
  )
  expect_match(out, "alpha tab content", fixed = TRUE)
  expect_match(out, "beta tab content", fixed = TRUE)
  expect_match(out, "alpha", fixed = TRUE)
  expect_match(out, "beta", fixed = TRUE)
  # tabsets render as a bootstrap tab-pane/nav structure, not literal
  # ".panel-tabset" text, so check for the nav machinery pandoc produces
  expect_match(out, "nav-tabs|tab-pane", perl = TRUE)
})

test_that("quarto_div renders its CSS class via quarto render", {
  skip_if_no_quarto()
  out <- render_quarto_chunk(
    'knitr::knit_print(quarto_div("Quartose div content", class = "quartose-test-div"))'
  )
  expect_match(out, "Quartose div content", fixed = TRUE)
  expect_match(out, "quartose-test-div", fixed = TRUE)
})

test_that("quarto_span renders its CSS class via quarto render", {
  skip_if_no_quarto()
  out <- render_quarto_chunk(
    'knitr::knit_print(quarto_span("Quartose span content", class = "quartose-test-span"))'
  )
  expect_match(out, "Quartose span content", fixed = TRUE)
  expect_match(out, "quartose-test-span", fixed = TRUE)
})

test_that("quarto_markdown passes raw markdown through via quarto render", {
  skip_if_no_quarto()
  out <- render_quarto_chunk(
    'knitr::knit_print(quarto_markdown(list("- quartose markdown item one", "- quartose markdown item two"), sep = "\\n"))'
  )
  # raw markdown list syntax should be parsed into an actual <ul>/<li>
  expect_match(out, "quartose markdown item one", fixed = TRUE)
  expect_match(out, "quartose markdown item two", fixed = TRUE)
  expect_match(out, "<li>", fixed = TRUE)
})

test_that("quarto_group renders all of its contained objects via quarto render", {
  skip_if_no_quarto()
  out <- render_quarto_chunk(
    paste(
      "grp <- quarto_group(list(",
      '  quarto_div("Quartose group div", class = "quartose-test-group-div"),',
      '  quarto_span("Quartose group span", class = "quartose-test-group-span")',
      "))",
      "knitr::knit_print(grp)",
      sep = "\n"
    )
  )
  expect_match(out, "Quartose group div", fixed = TRUE)
  expect_match(out, "Quartose group span", fixed = TRUE)
})

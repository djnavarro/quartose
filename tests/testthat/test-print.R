
test_objects <- list(
  q_section = quarto_section(title = "title", level = 2),
  q_tabset = quarto_tabset(content = list("content"), level = 2, names = "name"),
  q_div = quarto_div(content = list("content"), class = "column-margin", sep = ""),
  q_span = quarto_span(content = "text", class = "underline", sep = ""),
  q_group = quarto_group(content = list(quarto_section(title = "title", level = 2))),
  q_markdown = quarto_markdown(content = list("- this is a", "- markdown list"))
)

silent_print <- purrr::quietly(print)
silent_knit_print <- purrr::quietly(knitr::knit_print)

test_that("quarto objects can be printed", {

  for(oo in test_objects) {
    expect_no_error(silent_print(oo)) 
    expect_identical(silent_print(oo)$result, oo)
  }

})

test_that("quarto objects can be knitted", {

  for(oo in test_objects) {
    expect_no_error(silent_knit_print(oo))
    expect_null(silent_knit_print(oo)$result)
  }

})


test_sections <- list(
  quarto_section(.title = "Hello", .level = 2L),
  quarto_section(.title = "", .level = 6L)
)

test_that("quarto_section objects can be formatted", {
  for(ss in test_sections) {
    expect_no_error(format(ss))
  }
})

formatted_sections <- purrr::map(test_sections, format)

test_that("quarto_section objects format to strings", {
  for(ff in formatted_sections) {
    expect_true(rlang::is_character(ff))
    expect_length(ff, 1L)
  }
})

test_that("formatted quarto sections have correct structure", {
  for(i in seq_along(test_sections)) {
    ff <- formatted_sections[[i]]
    ss <- test_sections[[i]]
    r1 <- paste0("^\\n\\n#{", ss$level ,"}") # check how it starts
    r2 <- paste0("\\n\\n$") # check how it ends
    expect_match(object = ff, regexp = r1)
    expect_match(object = ff, regexp = r2)
  }
})



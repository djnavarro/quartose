
# basic checks ----------------------------------------

test_sections <- list(
  quarto_section(.title = "Hello", .level = 2L),
  quarto_section(.title = "", .level = 6L)
)

test_spans <- list(
  quarto_span(.content = "Hello", .class = "underline"),
  quarto_span(.content = "Hello", .class = c("underline", "mark")),
  quarto_span(.content = "Hello", .class = NULL),
  quarto_span(.content = c("Hello", "world"), .class = "underline", .sep = " "),
  quarto_span(.content = c("Hello", "world"), .class = "underline", .sep = "\n")
)

test_that("quarto_section objects can be formatted", {
  for(ss in test_sections) {
    expect_no_error(format(ss))
  }
})

test_that("quarto_span objects can be formatted", {
  for(ss in test_spans) {
    expect_no_error(format(ss))
  }
})

formatted_sections <- purrr::map(test_sections, format)
formatted_spans <- purrr::map(test_spans, format)

# sections --------------------------------------------

test_that("quarto_section objects format to strings", {
  for(ff in formatted_sections) {
    expect_true(rlang::is_character(ff))
    expect_length(ff, 1L)
  }
})

test_that("formatted quarto_section objects have correct structure", {
  for(i in seq_along(test_sections)) {
    ff <- formatted_sections[[i]]
    ss <- test_sections[[i]]
    r1 <- paste0("^\\n\\n#{", ss$level ,"}") # check how it starts
    r2 <- paste0("\\n\\n$") # check how it ends
    expect_match(object = ff, regexp = r1)
    expect_match(object = ff, regexp = r2)
  }
})

# tabsets --------------------------------------------


# divs -----------------------------------------------


# spans ----------------------------------------------

test_that("quarto_span objects format to strings", {
  for(ff in formatted_spans) {
    expect_true(rlang::is_character(ff))
    expect_length(ff, 1L)
  }
})

test_that("formatted quarto_span objects have correct structure", {

  for(i in seq_along(test_spans)) {
    ff <- formatted_spans[[i]]
    ss <- test_spans[[i]]

    # class and content from formatted spans
    ff_class   <- gsub("^\\[.*\\]", "", ff) # should be class info only, e.g. "{.underline}"
    ff_content <- gsub("\\{.*\\}$", "", ff) # should be content only, e.g., "[Hello]"
    
    # check that all content and class appear in the correct parts (but recall this test 
    # will skip cases when ss$class is NULL)
    for (co in ss$content) expect_match(ff_content, co, fixed = TRUE)
    for (cl in ss$class) expect_match(ff_class, paste0(".", cl), fixed = TRUE)
    
    # check that the collapsed versions are correct
    if (!rlang::is_null(ss$class)) cl <- paste0(".", ss$class)
    else cl <- ".quartose-null"
    expect_equal(
      object = ff_class, 
      expected = paste0("{", paste(cl, collapse = " "), "}")
    )
    expect_equal(
      object = ff_content, 
      expected = paste0("[", paste(ss$content, collapse = ss$sep), "]")
    )

  }
})

# groups/markdown ------------------------------------

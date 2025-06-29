test_that("quarto objects can be defined", {
  expect_no_error(quarto_section(.title = "title", .level = 2))
  expect_no_error(quarto_tabset(.content = list("content"), .level = 2, .names = "name"))
  expect_no_error(quarto_div(.content = list("content"), .class = "column-margin", .sep = ""))
  expect_no_error(quarto_span(.content = "text", .class = "underline", .sep = ""))
  expect_no_error(quarto_group(quarto_section(.title = "title", .level = 2)))
  expect_no_error(quarto_markdown("- this is a", "- markdown list")
)
})

q_section <- quarto_section(.title = "title", .level = 2)
q_tabset <- quarto_tabset(.content = list("content"), .level = 2, .names = "name")
q_div <- quarto_div(.content = list("content"), .class = "column-margin", .sep = "")
q_span <- quarto_span(.content = "text", .class = "underline", .sep = "")
q_group <- quarto_group(quarto_section(.title = "title", .level = 2))
q_markdown <- quarto_markdown("- this is a", "- markdown list")

test_that("quarto objects have correct S3 class", {

  expect_s3_class(q_section, "quarto_section")
  expect_s3_class(q_tabset, "quarto_tabset")
  expect_s3_class(q_div, "quarto_div")
  expect_s3_class(q_span, "quarto_span")
  expect_s3_class(q_group, "quarto_group")
  expect_s3_class(q_markdown, "quarto_markdown")

  expect_s3_class(q_section, "quarto_object")
  expect_s3_class(q_tabset, "quarto_object")
  expect_s3_class(q_div, "quarto_object")
  expect_s3_class(q_span, "quarto_object")
  expect_s3_class(q_group, "quarto_object")
  expect_s3_class(q_markdown, "quarto_object")

})

test_that("quarto object elements have the expected names", {

  expect_named(q_section, c("title", "level"))
  expect_named(q_tabset, c("content", "title", "names", "level"))
  expect_named(q_div, c("content", "class", "sep"))
  expect_named(q_span, c("content", "class", "sep"))
  expect_named(q_group, c("content"))
  expect_named(q_markdown, c("content", "sep"))

})
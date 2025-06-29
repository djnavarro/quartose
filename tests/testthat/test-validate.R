

# test the class validators -------------------------------------------------

test_that("quarto objects are detected by is_quarto", {
  obj <- list(
    q_section = quarto_section(.title = "title", .level = 2),
    q_tabset = quarto_tabset(.content = list("content"), .level = 2, .names = "name"),
    q_div = quarto_div(.content = list("content"), .class = "column-margin", .sep = ""),
    q_span = quarto_span(.content = "text", .class = "underline", .sep = ""),
    q_output = quarto_output(quarto_section(.title = "title", .level = 2)),
    q_markdown = quarto_markdown("text")
  )
  for (q in obj) expect_true(is_quarto(q))
})

test_that("ggplot objects are detected by is_ggplot", {
  p <- ggplot2::ggplot()
  expect_true(is_ggplot(p))
}) 
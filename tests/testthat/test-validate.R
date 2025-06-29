
# check that valid inputs are not rejected -----------------------------------

q_section <- quarto_section(.title = "title", .level = 2)
q_tabset <- quarto_tabset(.content = list("content"), .level = 2, .names = "name")
q_div <- quarto_div(.content = list("content"), .class = "column-margin", .sep = "")
q_span <- quarto_span(.content = "text", .class = "underline", .sep = "")
q_output <- quarto_output(quarto_section(.title = "title", .level = 2))
q_markdown <- quarto_markdown("- this is a", "- markdown list")

valid_titles <- list("cat", "", "<^&3 dsc")
valid_levels <- list(1L, 1.0, 1, 6L, 6.0, 6)
valid_content_tabset <- list(
  list("this list is one string"),
  list("this list is two strings", "see?"),
  list("this list", c("contains two", "character vectors")),
  list("this list is mixed", 2L, TRUE, c(1.2, 1.9)),
  list("this list has images", ggplot2::ggplot()),
  list("this list has lists", list(x = 1, y = 2)),
  list("this list has dates", as.Date("2025-10-10")),
  list("this list has data frames", data.frame(x = 1:2, y = 1:2)),
  list("this list contains model objects", lm(lm(Sepal.Length ~ Sepal.Width, iris))),
  list("this list contains a quarto_div", q_div),
  list("this list contains a quarto_span", q_span),
  list("this list contains a quarto_markdown", q_markdown)
)
valid_content_div <- list(
  list("this list is one string"),
  list("this list is two strings", "see?"),
  list("this list contains a span", q_span)
)
valid_content_span <- list(
  "this is one string",
  c("this is a vector of two strings", "see?")
)
valid_classes <- list(
  "column-margin",
  "callout-note",
  c("callout-note", "column-margin")
)
valid_sep <- list(
  "",
  " ",
  "\n"
)

test_that("valid quarto_section arguments are permitted", {

  for (ll in valid_levels) {
    for (tt in valid_titles) {
      expect_no_error(check_args_section(.title = tt, .level = ll))
      expect_no_error(quarto_section(.title = tt, .level = ll))
    }
  }

})

test_that("valid quarto_tabset arguments are permitted (explicit args)", {

  for (cc in valid_content_tabset) {
    valid_names_tabset <- list(
      rep("xxx", length(cc)),
      rep("", length(cc))
    )
    for (nn in valid_names_tabset) {
      for (tt in valid_titles) {
        for (ll in valid_levels) {
          expect_no_error(check_args_tabset(.content = cc, .title = tt, .level = ll, .names = nn))
          expect_no_error(quarto_tabset(.content = cc, .title = tt, .level = ll, .names = nn))
        }
      }
    }
  }

})

test_that("valid quarto_tabset arguments are permitted (indirect .names)", {

  tt <- valid_titles[[1]]
  ll <- valid_levels[[1]]
  for (cc in valid_content_tabset) {
    valid_names_tabset <- list(
      rep("xxx", length(cc)),
      rep("", length(cc))
    )
    for (nn in valid_names_tabset) {
      names(cc) <- nn
      for (ll in valid_levels) {
        expect_no_error(quarto_tabset(.content = cc, .title = tt, .level = ll))
      }
    }
  }
  
})

test_that("valid quarto_tabset arguments are permitted (null .title)", {

  ll <- valid_levels[[1]]
  cc <- valid_content_tabset[[1]]
  nn <- rep("xxx", length(cc))

  expect_no_error(check_args_tabset(.content = cc, .title = NULL, .level = ll, .names = nn))
  expect_no_error(quarto_tabset(.content = cc, .title = NULL, .level = ll, .names = nn))
  
})


test_that("valid quarto_div arguments are permitted", {

  for (co in valid_content_div) {
    for (cl in valid_classes) {
      for (ss in valid_sep) {
        expect_no_error(check_args_div(.content = co, .class = cl, .sep = ss))
        expect_no_error(quarto_div(.content = co, .class = cl, .sep = ss))
      }
    }
  }

})
  
test_that("valid quarto_span arguments are permitted", {

  for (co in valid_content_span) {
    for (cl in valid_classes) {
      for (ss in valid_sep) {
        expect_no_error(check_args_span(.content = co, .class = cl, .sep = ss))
        expect_no_error(quarto_span(.content = co, .class = cl, .sep = ss))
      }
    }
  }
  
})

test_that("valid quarto_output arguments are permitted", {

  expect_no_error(check_args_output(q_div))
  expect_no_error(quarto_output(q_div))

  expect_no_error(check_args_output(q_tabset))
  expect_no_error(quarto_output(q_tabset))

  expect_no_error(check_args_output(q_div, q_tabset))
  expect_no_error(quarto_output(q_div, q_tabset))

})

test_that("valid quarto_markdown arguments are permitted", {

  for (ss in valid_sep) {

    expect_no_error(check_args_markdown("just one string", .sep = ss))
    expect_no_error(quarto_markdown("just one string", .sep = ss))

    expect_no_error(check_args_markdown("- each list item", "- is a string", .sep = ss))
    expect_no_error(quarto_markdown("- each list item", "- is a string", .sep = ss))

    expect_no_error(check_args_markdown(c("- one character vector", "- with **bold**"), .sep = ss))
    expect_no_error(quarto_markdown(c("- one character vector", "- with **bold**"), .sep = ss))

    expect_no_error(check_args_markdown("two character vectors", c("of different", "lengths"), .sep = ss))
    expect_no_error(quarto_markdown("two character vectors", c("of different", "lengths"), .sep = ss))

  }

})


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


# check that invalid quarto_section inputs are rejected -----------------------------------

ll <- valid_levels[[1]]
tt <- valid_titles[[1]]

test_that("invalid quarto_section .title arguments throw errors", {

  expect_error(quarto_section(.title = c("too", "long"), .level = ll))
  expect_error(quarto_section(.title = character(0L), .level = ll)) # too short

})

test_that("invalid quarto_section .level arguments throw errors", {

  expect_error(quarto_section(.title = tt, .level = "wrong type"))
  expect_error(quarto_section(.title = tt, .level = 1.343234))
  expect_error(quarto_section(.title = tt, .level = c(2L, 5L))) # too long

  expect_error(quarto_section(.title = tt, .level = -10L)) # below minimum 
  expect_error(quarto_section(.title = tt, .level = 0L))   # below minimum 
  expect_error(quarto_section(.title = tt, .level = 7L))   # above maximum
  
})

# check that invalid quarto_tabset inputs are rejected -----------------------------------

test_that("invalid quarto_tabset .content arguments throw errors", {

  # .content is wrong type (but .names would be fine if .content were okay)
  expect_error(quarto_tabset(.content = "not a list", .title = tt, .level = ll, .names = "name is consistent"))
  expect_error(quarto_tabset(.content = 2L, .title = tt, .level = ll, .names = "name is consistent"))
  expect_error(quarto_tabset(.content = character(0L), .title = tt, .level = ll, .names = character(0L)))
  expect_error(quarto_tabset(.content = NULL, .title = tt, .level = ll, .names = NULL))

})

test_that("invalid quarto_tabset .name arguments throw errors", {

  # .name is wrong type (but .content would be fine if .names were okay)
  expect_error(quarto_tabset(.content = list("this is okay"), .title = tt, .level = ll, .names = 2L))
  expect_error(quarto_tabset(.content = list("this is okay"), .title = tt, .level = ll, .names = list("this is not okay")))

})

test_that("inconsistent .name and .content arguments to quarto_tabset throws errors", {

  expect_error(quarto_tabset(.content = list("this is okay"), .title = tt, .level = ll, .names = c("but this is", "too long")))
  expect_error(quarto_tabset(.content = list("this is okay"), .title = tt, .level = ll, .names = character(0L)))

})

cc <- list("this content is okay")
nn <- "this name is also okay"
tt <- "this title is also okay" 
ll <- 2L

test_that("invalid quarto_tabset .level arguments throw errors", {

  expect_error(quarto_tabset(.content = cc, .names = nn, .title = tt, .level = "wrong type"))
  expect_error(quarto_tabset(.content = cc, .names = nn, .title = tt, .level = 1.343234))
  expect_error(quarto_tabset(.content = cc, .names = nn, .title = tt, .level = c(2L, 5L))) # too long

  expect_error(quarto_tabset(.content = cc, .names = nn, .title = tt, .level = -10L)) # below minimum 
  expect_error(quarto_tabset(.content = cc, .names = nn, .title = tt, .level = 0L))   # below minimum 
  expect_error(quarto_tabset(.content = cc, .names = nn, .title = tt, .level = 7L))   # above maximum
  
})

test_that("invalid quarto_tabset .title arguments throw errors", {

  expect_error(quarto_tabset(.content = cc, .names = nn, .title = c("too", "long"), .level = ll))
  expect_error(quarto_tabset(.content = cc, .names = nn, .title = character(0L), .level = ll)) # too short

})

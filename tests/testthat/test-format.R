
# basic checks ------------------------------------------------------------

test_sections <- list(
  quarto_section(title = "Hello", level = 2L),
  quarto_section(title = "", level = 6L)
)

test_spans <- list(
  quarto_span(content = "Hello", class = "underline"),
  quarto_span(content = "Hello", class = c("underline", "mark")),
  quarto_span(content = "Hello", class = NULL),
  quarto_span(content = c("Hello", "world"), class = "underline", sep = " "),
  quarto_span(content = c("Hello", "world"), class = "underline", sep = "\n")
)

test_divs <- list(
  quarto_div(content = "Hello", class = "column-margin"),
  quarto_div(content = "Hello", class = c("column-margin", "callout-tip")),
  quarto_div(content = "Hello", class = NULL),
  quarto_div(content = c("Hello", "world"), class = "column-margin", sep = " "),
  quarto_div(content = c("Hello", "world"), class = "column-margin", sep = "\n"),
  quarto_div(content = list("Hello", "world"), class = "column-margin", sep = " "),
  quarto_div(
    content = list(
      quarto_span(content = "this is plain text"),
      quarto_span(content = "this is highlighted", class = "mark"),
      quarto_span(content = "this is underlined", class = "underline")
    ),
    sep = ", "
  )
)

test_markdown <- list(
  quarto_markdown(content = list("item", "item"), sep = " "),
  quarto_markdown(content = c("item", "item"), sep = " "),
  quarto_markdown(content = c("item", "item"), sep = "\n")
)

test_tabsets <- list(
  quarto_tabset(content = list("text", "text"), names = c("name", "name"), level = 2L),
  quarto_tabset(content = list("text", "text"), title = "title", names = c("name", "name"), level = 2L),
  quarto_tabset(content = list("text", "text"), names = c("name", "name"), level = 6L),
  quarto_tabset(content = list("text", "text", "text"), names = c("name", "name", "name"), level = 6L),
  quarto_tabset(content = list(x = "text", y = "text"), level = 6L),
  quarto_tabset(content = list(a = 1:10, b = LETTERS, c = list(1:2, 3:4)), level = 2L),
  quarto_tabset(
    content = list(
      a = lm(Sepal.Width ~ Sepal.Length, iris),
      b = LETTERS, 
      c = list(1:2, 3:4)
    ), 
    level = 2L
  )
)

if (requireNamespace("ggplot2", quietly = TRUE)) {
  tt <- quarto_tabset(
    content = list(
      a = ggplot2::ggplot(),
      b = lm(Sepal.Width ~ Sepal.Length, iris)
    ), 
    level = 2L
  )
  ll <- length(test_tabsets)
  test_tabsets[[ll + 1]] <- tt
}

test_groups <- list(
  quarto_group(list(
    quarto_tabset(content = list("text", "text"), names = c("name", "name"), level = 2L),
    quarto_tabset(content = list("text", "text"), title = "title", names = c("name", "name"), level = 2L)  
  )),
  quarto_group(list(
    quarto_tabset(content = list("text", "text"), names = c("name", "name"), level = 6L),
    quarto_tabset(content = list("text", "text", "text"), names = c("name", "name", "name"), level = 6L),
    quarto_tabset(content = list(x = "text", y = "text"), level = 6L)
  ))
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

test_that("quarto_div objects can be formatted", {
  for(dd in test_divs) {
    expect_no_error(format(dd))
  }
})

test_that("quarto_markdown objects can be formatted", {
  for(mm in test_markdown) {
    expect_no_error(format(mm))
  }
})

test_that("quarto_tabset objects can be formatted", {
  for(tt in test_tabsets) {
    expect_no_error(format(tt))
  }
})

test_that("quarto_group objects can be formatted", {
  for(gg in test_groups) {
    expect_no_error(format(gg))
  }
})

formatted_sections <- purrr::map(test_sections, format)
formatted_spans <- purrr::map(test_spans, format)
formatted_divs <- purrr::map(test_divs, format)
formatted_markdown <- purrr::map(test_markdown, format)
formatted_tabsets <- purrr::map(test_tabsets, format)
formatted_groups <- purrr::map(test_groups, format)

# sections ---------------------------------------------------------

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

# spans ------------------------------------------------------------

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

# divs -----------------------------------------------

test_that("quarto_div objects format to strings", {
  for(dd in formatted_divs) {
    expect_true(rlang::is_character(dd))
    expect_length(dd, 1L)
  }
})

test_that("formatted quarto_div objects have correct structure", {
  for(i in seq_along(test_divs)) {
    ff <- formatted_divs[[i]]
    dd <- test_divs[[i]]
    expect_match(object = ff, regexp = "^\n\n::: \\{") # check start
    expect_match(object = ff, regexp = "\n\n:::\n\n$") # check end
    if (is.null(dd$class)) dd$class <- "quartose-null"
    cl <- paste0(".", dd$class, collapse = " ")
    cl <- paste0("\\{", cl, "\\}")
    expect_match(object = ff, regexp = cl) # check for class string
  }
})


# markdown ---------------------------------------------------------

test_that("quarto_markdown objects format to strings", {
  for(mm in formatted_markdown) {
    expect_true(rlang::is_character(mm))
    expect_length(mm, 1L)
  }
})

test_that("formatted quarto_markdown objects have correct structure", {
  for(i in seq_along(test_markdown)) {
    ff <- formatted_markdown[[i]]
    mm <- test_markdown[[i]]
    expect_equal(ff, paste(mm$content, collapse = mm$sep))
  }
})

# tabsets ----------------------------------------------------------

test_that("formatted quarto_tabset objects are character/graphics lists", {
  for(i in seq_along(test_tabsets)) {
    tt <- test_tabsets[[i]]
    ff <- formatted_tabsets[[i]]
    expect_true(rlang::is_list(ff))
    expect_true(
      all(purrr::map_lgl(ff, function(x) {
        rlang::is_bare_character(x) | inherits(x, "quarto_plot")
      }))
    )
    ff_text <- purrr::map(ff, function(x) {
      if (rlang::is_bare_character(x)) return(x)
      "PLOT"      
    })
    expect_true(all(purrr::map_int(ff_text, length) == 1L))
  }
})

test_that("character elements of formatted quarto_tabset are length 1", {
  for(i in seq_along(test_tabsets)) {
    tt <- test_tabsets[[i]]
    ff <- formatted_tabsets[[i]]
    ff_text <- purrr::map(ff, function(x) {
      if (rlang::is_bare_character(x)) return(x)
      "PLOT"      
    })
    expect_true(all(purrr::map_int(ff_text, length) == 1L))
  }
})

test_that("formatted quarto_tabsets include .panel-tabset div", {
  for(i in seq_along(test_tabsets)) {
    tt <- test_tabsets[[i]]
    ff <- formatted_tabsets[[i]]
    ff_chr <- unname(purrr::map_chr(ff, function(x) {
      if (rlang::is_bare_character(x)) return(x)
      "PLOT"      
    }))
    ind <- 1L
    if (!is.null(tt$title)) ind <- 2L
    expect_equal(ff_chr[ind], "\n\n::: {.panel-tabset}\n\n") # div open
    expect_equal(ff_chr[length(ff_chr)], "\n\n::: \n\n") # div close
  }
})

test_that("formatted quarto_tabsets include section/tab titles", {
  for(i in seq_along(test_tabsets)) {
    tt <- test_tabsets[[i]]
    ff <- formatted_tabsets[[i]]
    ff_chr <- unname(purrr::map_chr(ff, function(x) {
      if (rlang::is_bare_character(x)) return(x)
      "PLOT"      
    }))

    # the title should be there
    if (!is.null(tt$title)) {
      expect_match(ff_chr[1L], tt$title, fixed = TRUE)
    }
    # at least one formatted string should match each name
    for (nn in seq_along(tt$names)) {
      expect_true(length(grep(tt$names[nn], ff_chr)) > 0L)
    }
  }
})

# groups -----------------------------------------------------------

test_that("formatted quarto_groups are lists", {
  for(i in seq_along(test_groups)) {
    gg <- test_groups[[i]]
    ff <- formatted_groups[[i]]
    expect_true(rlang::is_list(ff))
  }
})

# regression tests: html escaping and plot-class preservation (issue #1) ---

test_that("quarto_plot wrapper preserves its class when captured in a tabset", {
  skip_if_not_installed("ggplot2")
  tt <- quarto_tabset(content = list(a = ggplot2::ggplot()), level = 2L)
  ff <- format(tt)
  is_plot <- purrr::map_lgl(ff, function(x) inherits(x, "quarto_plot"))
  expect_true(any(is_plot))
  plot_el <- ff[[which(is_plot)[1]]]
  expect_s3_class(plot_el, "quarto_object")
  expect_s3_class(plot_el$content, "ggplot")
})

test_that("quarto_tabset content wrapped in a quarto_plot can be knit-printed", {
  skip_if_not_installed("ggplot2")
  tt <- quarto_tabset(content = list(a = ggplot2::ggplot()), level = 2L)
  expect_no_error(purrr::quietly(knitr::knit_print)(tt))
})

test_that("protect_angle_brackets escapes '<' and '>'", {
  expect_equal(
    quartose:::protect_angle_brackets("a <fct> b"),
    "a &lt;fct&gt; b"
  )
  expect_equal(quartose:::protect_angle_brackets("no brackets"), "no brackets")
})

test_that("quarto_tabset escapes angle brackets in captured output (#1)", {
  fake <- structure(list(), class = "quartose_test_fake")
  registerS3method("print", "quartose_test_fake", function(x, ...) cat("<fct>\n"))

  tt <- quarto_tabset(content = list(a = fake), level = 2L)
  ff <- format(tt)
  ff_chr <- unlist(ff[purrr::map_lgl(ff, rlang::is_bare_character)])

  expect_false(any(grepl("<fct>", ff_chr, fixed = TRUE)))
  expect_true(any(grepl("&lt;fct&gt;", ff_chr, fixed = TRUE)))
})

# generalized graphics support (issue #2) -----------------------------------

test_that("quarto_tabset wraps recorded plots and grobs in quarto_plot, like ggplot", {

  rp <- make_recorded_plot()

  tt <- quarto_tabset(content = list(a = rp, b = grid::rectGrob()), level = 2L)
  ff <- format(tt)
  is_plot <- purrr::map_lgl(ff, function(x) inherits(x, "quarto_plot"))

  expect_equal(sum(is_plot), 2L)
  plot_els <- ff[is_plot]
  expect_s3_class(plot_els[[1]]$content, "recordedplot")
  expect_s3_class(plot_els[[2]]$content, "grob")

})

test_that("recorded plots and grobs in a tabset can be knit-printed", {

  rp <- make_recorded_plot()

  tt <- quarto_tabset(content = list(a = rp, b = grid::rectGrob()), level = 2L)
  expect_no_error(purrr::quietly(knitr::knit_print)(tt))

})

test_that("render_graphic_png renders recorded plots and grobs to a real file", {

  rp <- make_recorded_plot()

  out1 <- quartose:::render_graphic_png(rp)
  expect_true(file.exists(out1))
  expect_gt(file.size(out1), 0)

  out2 <- quartose:::render_graphic_png(grid::rectGrob())
  expect_true(file.exists(out2))
  expect_gt(file.size(out2), 0)

})

if (requireNamespace("lattice", quietly = TRUE)) {
  test_that("trellis objects in a tabset are detected and knit-print without error", {
    tt <- quarto_tabset(content = list(a = lattice::xyplot(1 ~ 1)), level = 2L)
    ff <- format(tt)
    expect_true(any(purrr::map_lgl(ff, function(x) inherits(x, "quarto_plot"))))
    expect_no_error(purrr::quietly(knitr::knit_print)(tt))
  })
}

if (requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("patchwork", quietly = TRUE)) {
  test_that("patchwork objects in a tabset are handled via the existing ggplot path", {
    combined <- ggplot2::ggplot() + ggplot2::ggplot()
    tt <- quarto_tabset(content = list(a = combined), level = 2L)
    ff <- format(tt)
    is_plot <- purrr::map_lgl(ff, function(x) inherits(x, "quarto_plot"))
    expect_true(any(is_plot))
    expect_s3_class(ff[[which(is_plot)[1]]]$content, "ggplot")
    expect_no_error(purrr::quietly(knitr::knit_print)(tt))
  })
}

test_that("as_quarto_graphic() lets an unrecognized graphics object be captured in a tabset", {

  obj <- structure(list(), class = "quartose_test_baseplot")
  registerS3method("print", "quartose_test_baseplot", function(x, ...) plot(1:3))

  tt <- quarto_tabset(content = list(a = as_quarto_graphic(obj)), level = 2L)
  ff <- format(tt)
  is_plot <- purrr::map_lgl(ff, function(x) inherits(x, "quarto_plot"))

  expect_true(any(is_plot))
  expect_s3_class(ff[[which(is_plot)[1]]]$content, "quartose_graphic")
  expect_no_error(purrr::quietly(knitr::knit_print)(tt))

})

# graphics support in quarto_div() (issue #3) -------------------------------

test_that("quarto_div without graphics still formats to a single string", {
  dd <- quarto_div(content = list("plain text", "more text"), sep = " ")
  ff <- format(dd)
  expect_true(rlang::is_character(ff))
  expect_length(ff, 1L)
})

test_that("quarto_div wraps recorded plots and grobs in quarto_plot, like quarto_tabset", {

  rp <- make_recorded_plot()

  dd <- quarto_div(content = list(rp, grid::rectGrob()), sep = " ")
  ff <- format(dd)

  expect_true(rlang::is_list(ff))
  is_plot <- purrr::map_lgl(ff, function(x) inherits(x, "quarto_plot"))
  expect_equal(sum(is_plot), 2L)

  plot_els <- ff[is_plot]
  expect_s3_class(plot_els[[1]]$content, "recordedplot")
  expect_s3_class(plot_els[[2]]$content, "grob")

  # div open/close markup still bookends the list
  expect_true(grepl("^\n\n::: \\{", ff[[1]]))
  expect_true(grepl("\n\n:::\n\n$", ff[[length(ff)]]))

})

test_that("a mix of text and a graphic in a quarto_div formats to a list, in order", {

  rp <- make_recorded_plot()

  dd <- quarto_div(content = list("before", rp, "after"), sep = " | ")
  ff <- format(dd)

  is_plot <- purrr::map_lgl(ff, function(x) inherits(x, "quarto_plot"))
  expect_equal(sum(is_plot), 1L)

  ff_chr <- unlist(ff[!is_plot])
  expect_true(any(grepl("before", ff_chr, fixed = TRUE)))
  expect_true(any(grepl("after", ff_chr, fixed = TRUE)))
  expect_true(any(grepl("\\|", ff_chr))) # the separator appears between elements

})

test_that("a quarto_div containing a graphic can be knit-printed", {
  rp <- make_recorded_plot()
  dd <- quarto_div(content = list("caption", rp), sep = " ")
  expect_no_error(purrr::quietly(knitr::knit_print)(dd))
})

test_that("as_quarto_graphic() lets an unrecognized graphics object be captured in a div", {

  obj <- structure(list(), class = "quartose_test_baseplot_div")
  registerS3method("print", "quartose_test_baseplot_div", function(x, ...) plot(1:3))

  dd <- quarto_div(content = list(as_quarto_graphic(obj)))
  ff <- format(dd)
  is_plot <- purrr::map_lgl(ff, function(x) inherits(x, "quarto_plot"))

  expect_true(any(is_plot))
  expect_s3_class(ff[[which(is_plot)[1]]]$content, "quartose_graphic")
  expect_no_error(purrr::quietly(knitr::knit_print)(dd))

})

if (requireNamespace("ggplot2", quietly = TRUE)) {
  test_that("ggplot objects in a div are handled via the existing ggplot path", {
    dd <- quarto_div(content = list(ggplot2::ggplot()))
    ff <- format(dd)
    is_plot <- purrr::map_lgl(ff, function(x) inherits(x, "quarto_plot"))
    expect_true(any(is_plot))
    expect_s3_class(ff[[which(is_plot)[1]]]$content, "ggplot")
    expect_no_error(purrr::quietly(knitr::knit_print)(dd))
  })
}

test_that("formatted quarto_groups concatenate their formatted constituents", {
  for(i in seq_along(test_groups)) {
    gg <- test_groups[[i]]
    ff <- formatted_groups[[i]]
    ll <- list()
    for(cc in gg$content) {
      ll <- c(ll, format(cc))
    }
    expect_equal(length(ff), length(ll)) # same length
    expect_identical(ff, ll) # actually the same
  }
})

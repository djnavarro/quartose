
# tabs and sections -------------------------------------------

#' @exportS3Method knitr::knit_print
knit_print.quarto_section <- function(x, ...) {
  hashes <- paste(rep("#", x$level), collapse = "")
  header <- paste0("\n\n", hashes, " ", x$title, "\n\n")
  cat(header)
}

#' @exportS3Method knitr::knit_print
knit_print.quarto_tabset <- function(x, ...) {

  # open tabset
  cat("\n\n::: {.panel-tabset}\n\n")
  for(i in seq_along(x$content)) {

    # create tab with section header
    hashes <- paste(rep("#", x$level), collapse = "")
    header <- paste0("\n\n", hashes, " ", x$title[i], "\n\n")
    cat(header)

    # output
    if (inherits(x$content[[i]], "ggplot")) { # TODO: maybe use is_ggplot
      knitr::knit_print(x$content[[i]])
    } else {
      cat("<pre>")
      knitr::knit_print(x$content[[i]])
      cat("</pre>")
    }

  }

  # close tabset
  cat("\n\n::: \n\n")
}

#' @exportS3Method knitr::knit_print
knit_print.quarto_tabsec <- function(x, ...) {
  knitr::knit_print(quarto_section(.title = x$title, .level = x$level))
  knitr::knit_print(quarto_tabset(.content = x$content, .level = x$level + 1))
}

# groups of output -------------------------------------------

# An "output group" expects to be treated as if it were normal
# output of R code (even though technically the whole thing is)
# inside "results: asis". To address this, the print method 
# for the group iterates over each content item and calls the
# corresponding knit_print() method. It works well for sequentially
# printing multiple code chunk outouts

#' @exportS3Method knitr::knit_print
knit_print.quarto_output <- function(x, ...) {
  purrr::walk(x$content, knitr::knit_print)
} 

# A "markdown group" expects to be passed directly into the 
# document, as is, with no special instruction. To address this,
# the print method for the group treats each content item like
# a plain string, pastes them all together, and then bypasses 
# the usual knitr processing by using knitr::asis_output(). It
# works well when each content item is plain text, but that plain
# text is to be interpreted as markdown

#' @exportS3Method knitr::knit_print
knit_print.quarto_markdown <- function(x, ...) {
  str <- paste(unlist(x$content), collapse = x$sep)
  knitr::asis_output(str)
}

# A "paragraph group" is slightly different. It expects that
# each content item should be rendered to markdown/quarto 
# syntax (using knit_print), pasted together into a group, 
# and then passed directly to the document. It works well
# for grouping a collection of quarto_span objects

#' @exportS3Method knitr::knit_print
knit_print.quarto_paragraph <- function(x, ...) {
  str <- purrr::map_chr(x$content, \(c) {
    unclass(c(knitr::knit_print(c)))
  })
  cat(str, sep = x$sep)
}

# divs and spans -------------------------------------------

#' @exportS3Method knitr::knit_print
knit_print.quarto_div <- function(x, ...) {
  classes <- paste(".", x$class, sep = "", collapse = " ")
  open_div <- paste0("\n\n::: {", classes, "}\n\n")
  close_div <- "\n\n:::\n\n"
  content <- paste(unlist(x$content), collapse = x$sep)
  str <- paste(open_div, content, close_div, sep = "\n")
  knitr::asis_output(str)
}

#' @exportS3Method knitr::knit_print
knit_print.quarto_span <- function(x, ...) {
  if (is.null(x$class)) {
    str <- x$content
  } else {
    classes <- paste(".", x$class, sep = "", collapse = " ")
    str <- paste0("[", x$content, "]{", classes, "}")
  }
  knitr::asis_output(str)
}

# treat print the same as knit_print --------------------------

#' @exportS3Method base::print
print.quarto_section <- knit_print.quarto_section

#' @exportS3Method base::print
print.quarto_tabset <- knit_print.quarto_tabset

#' @exportS3Method base::print
print.quarto_tabsec <- knit_print.quarto_tabsec

#' @exportS3Method base::print
print.quarto_output <- knit_print.quarto_output

#' @exportS3Method base::print
print.quarto_markdown <- knit_print.quarto_markdown

#' @exportS3Method base::print
print.quarto_paragraph <- knit_print.quarto_paragraph

#' @exportS3Method base::print
print.quarto_div <- knit_print.quarto_div

#' @exportS3Method base::print
print.quarto_span <- knit_print.quarto_span


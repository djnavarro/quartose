
# notes:
#
# - it is assumed that the code chunk is "results: asis"
#
# - cat() passes the output directly to the document; the output
#   bypasses knitr formatting, but will be visible to the quarto 
#   parser, so if you pass a quarto directive with cat(), it will
#   convert to html
#
# - knit_print() applies knitr formatting to the output before 
#   passing it to the document; this will happen even though the
#   surrounding code chunk is "results: asis"; in effect it 
#   produces the "knitr formatted string". whether quarto parses
#   this result depends entirely on what the specific knit_print
#   method returns. in the usual case, quarto parsing doesn't 
#   happen. in the special case where knitr returns a protected
#   string via asis_output(), it behaves more like cat() because
#   the string is passed directly to the document and can be
#   parsed as markdown. however, this is fragile due to the 
#   limitations of asis_output(); it only works when it is 
#   invoked at the top level


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
    header <- paste("\n\n", hashes, " ", x$title[i], "\n\n", sep = "")
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
  str <- purrr::map_chr(x$content, \(cc) {
    unclass(c(knitr::knit_print(cc)))
  })
  cat(str, sep = x$sep)
}

# divs and spans -------------------------------------------

#' @exportS3Method knitr::knit_print
knit_print.quarto_div <- function(x, ...) {

  div_body <- purrr::map_chr(x$content, \(cc) {
    paste(
      strip_asis(knitr::knit_print(cc)), 
      collapse = x$sep
    )
  })
  div_body <- paste(div_body, collapse = x$sep)
  
  if (is.null(x$class)) {
    div_open <- "\n\n:::\n\n"
  } else {
    css_info <- paste(".", x$class, sep = "", collapse = " ")
    div_open <- paste("\n\n::: {", css_info, "}\n\n", sep = "", collapse = " ")
  }
  div_shut <- "\n\n:::\n\n"
  div <- paste(div_open, div_body, div_shut)

  knitr::asis_output(div)
}

#' @exportS3Method knitr::knit_print
knit_print.quarto_span <- function(x, ...) {

  span <- paste(unlist(x$content), sep = x$sep, collapse = x$sep)
  if (!is.null(x$class)) {
    css_info <- paste(".", x$class, sep = "", collapse = " ")
    span <- paste("[", span, "]{", css_info, "}", sep = "")
  }

  knitr::asis_output(span)
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

  
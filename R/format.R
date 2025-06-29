
# tabs and sections -------------------------------------------

#' @exportS3Method base::format
format.quarto_section <- function(x, ...) {

  hashes <- paste(rep("#", x$level), collapse = "")
  header <- paste0("\n\n", hashes, " ", x$title, "\n\n")

  return(header)
}

#' @exportS3Method base::format
format.quarto_tabset <- function(x, ...) {

  # section title
  if(!is.null(x$title)) {
    title <- format(quarto_section(x$title, x$level))
    x$level <- x$level + 1L
  } else {
    title <- character(0L)
  }

  # tab names
  tabname <- purrr::map_chr(x$names, \(tt) {
    format(quarto_section(tt, x$level))
  })

  # open and shut tabset (quarto)
  tabset_open <- "\n\n::: {.panel-tabset}\n\n"
  tabset_shut <- "\n\n::: \n\n"

  # open and shut pre (html)
  pre_open <- "<pre>"
  pre_shut <- "</pre>"

  # the iterative bit
  out <- list()

  out <- c(out, title)
  out <- c(out, tabset_open)
  for(i in seq_along(x$content)) {

    out <- c(out, tabname[i])

    # for plot objects, we do no formatting at this stage. all we do
    # is wrap them safely within a quarto_* class, so that later on
    # when the print method is called, it is handled as a quarto obj
    if (is_ggplot(x$content[[i]])) {
      out <- c(out, quarto_plot(.content = x$content[[i]]))
    
    # for everything else, call knit_print() now in order to produce 
    # character strings that can be passed directly to document with
    # cat(), but don't actually print them yet, just capture and store
    } else {
      out <- c(out, pre_open)
      out <- c(out, capture.output(knitr::knit_print(x$content[[i]])))
      out <- c(out, pre_shut)
    }

  }
  out <- c(out, tabset_shut)

  return(out)
}

# divs and spans -------------------------------------------

#' @exportS3Method base::format
format.quarto_div <- function(x, ...) {

  div_body <- format_elements(x$content)
  div_body <- paste(unlist(div_body), collapse = x$sep)
  
  if (is.null(x$class)) x$class <- "quarto_null"
  css_info <- paste(".", x$class, sep = "", collapse = " ")
  div_open <- paste("\n\n::: {", css_info, "}\n\n", sep = "", collapse = " ")
  div_shut <- "\n\n:::\n\n"

  div <- paste(div_open, div_body, div_shut)
  return(div)
}

#' @exportS3Method base::format
format.quarto_span <- function(x, ...) {

  span <- paste(unlist(x$content), sep = x$sep, collapse = x$sep)
  if (!is.null(x$class)) {
    css_info <- paste(".", x$class, sep = "", collapse = " ")
    span <- paste("[", span, "]{", css_info, "}", sep = "")
  }

  return(span)
}

# groups of output -------------------------------------------

#' @exportS3Method base::format
format.quarto_markdown <- function(x, ...) {
  md <- format_elements(x$content)
  md <- paste(unlist(md), collapse = x$sep)
  return(md)
}

#' @exportS3Method base::format
format.quarto_output <- function(x, ...) {
  out <- format_elements(x$content)
  out <- purrr::flatten(out)
  return(out)
} 

# formatting helpers -----------------------------------------

format_elements <- function(x, ...) {
  purrr::map(x, \(cc) format(cc, ...))
}


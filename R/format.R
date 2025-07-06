
# tabs and sections -------------------------------------------

#' @title Format a quarto object
#' 
#' @description
#' Creates a formatted representation of a quarto object in a 
#' form suitable for printing. When calling `knitr::knit_print()`
#' on a quarto object, the relevant `format()` method is called
#' first, and the formatted version is printed to the document.
#' Note that the base `print()` method for quarto objects does
#' not call `format()`. 
#' 
#' @param x A quarto object
#' @param ... Other arguments (ignored)
#' 
#' @return 
#' A formatted quarto object, sometimes a single string
#' (e.g., for `quarto_section` objects), but can be a list of
#' strings and/or plot objects (e.g., for `quarto_tabset` objects)
#' 
#' @details
#' The intent behind the `format()` methods for quarto objects
#' is to create a ready-to-print representation of that is almost
#' identical to what will be printed into the quarto document 
#' when the print method is called. Because of this, the formatted
#' version of a quarto object is usually a string or a list of 
#' strings, but it can also include plot objects that have not
#' yet been rendered. The resulting representation isn't usually
#' very pretty, though if passed to `cat()` it is generally
#' readable. 
#' 
#' @name format_quarto
#' 
#' @examples
#' # formatted sections, spans and divs
#' sec <- quarto_section("Header", level = 2L)
#' spn <- quarto_span("Content", class = "underline")
#' div <- quarto_div("Content", class = "content-margin")
#' 
#' format(sec)
#' format(spn)
#' format(div)
#' 
#' # formatted tabsets
#' tbs <- quarto_tabset(
#'   content = list(tab1 = 1:10, tab2 = "hello"),
#'   title = "Header",
#'   level = 2L
#' )
#' 
#' format(tbs)
#' 
#' # formatted groups and markdown
#' 
#' mkd <- quarto_markdown(list("- this is a", "- markdown list"), sep = "\n")
#' gps <- quarto_group(list(div, mkd))
#' 
#' format(mkd)
#' format(gps)
#' 
NULL

#' @rdname format_quarto
#' @exportS3Method base::format
format.quarto_section <- function(x, ...) {

  hashes <- paste(rep("#", x$level), collapse = "")
  header <- paste0("\n\n", hashes, " ", x$title, "\n\n")

  return(header)
}

#' @rdname format_quarto
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
      out <- c(out, quarto_plot(content = x$content[[i]]))
    
    # for everything else, call knit_print() now in order to produce 
    # character strings that can be passed directly to document with
    # cat(), but don't actually print them yet, just capture and store
    } else {
      out <- c(out, pre_open)
      out <- c(out, utils::capture.output(knitr::knit_print(x$content[[i]])))
      out <- c(out, pre_shut)
    }

  }
  out <- c(out, tabset_shut)

  return(out)
}

# divs and spans -------------------------------------------

#' @rdname format_quarto
#' @exportS3Method base::format
format.quarto_div <- function(x, ...) {

  div_body <- format_elements(x$content)
  div_body <- paste(unlist(div_body), collapse = x$sep)
  
  if (is.null(x$class) | !length(x$class)) x$class <- "quartose-null"
  css_info <- paste(".", x$class, sep = "", collapse = " ")
  div_open <- paste("\n\n::: {", css_info, "}\n\n", sep = "", collapse = " ")
  div_shut <- "\n\n:::\n\n"

  div <- paste(div_open, div_body, div_shut)
  return(div)
}

#' @rdname format_quarto
#' @exportS3Method base::format
format.quarto_span <- function(x, ...) {

  span_content <- paste(unlist(x$content), sep = x$sep, collapse = x$sep)
  if (is.null(x$class) | !length(x$class)) x$class <- "quartose-null"
  css_info <- paste(".", x$class, sep = "", collapse = " ")
  span <- paste("[", span_content, "]{", css_info, "}", sep = "")

  return(span)
}

# groups of output -------------------------------------------

#' @rdname format_quarto
#' @exportS3Method base::format
format.quarto_markdown <- function(x, ...) {
  md <- format_elements(x$content)
  md <- paste(unlist(md), collapse = x$sep)
  return(md)
}

#' @rdname format_quarto
#' @exportS3Method base::format
format.quarto_group <- function(x, ...) {
  out <- format_elements(x$content)
  out <- purrr::flatten(out)
  return(out)
} 

# formatting helpers -----------------------------------------

format_elements <- function(x, ...) {
  purrr::map(x, \(cc) format(cc, ...))
}


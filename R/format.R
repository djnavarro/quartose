
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
#' @param x A quarto object.
#' @param ... Other arguments (ignored).
#' 
#' @return 
#' A formatted quarto object. For `quarto_section`, `quarto_span`, 
#' and `quarto_markdown` objects, the formatted output is always a
#' string (character vector of length 1). For `quarto_tabset` and
#' `quarto_group` objects, the output is always a list whose elements
#' are either strings or plot objects. For `quarto_div` objects,
#' the output is currently a string, but this may change to list 
#' output in future if divs are permitted to contain plots. 
#'  
#' @details
#' The intent behind the `format()` methods for quarto objects
#' is to create a ready-to-print representation of that is almost
#' identical to what will be printed into the quarto document 
#' when `knitr::knit_print()` is called. Because of this, the 
#' formatted version of a quarto object is a string or a list of 
#' strings, but it may also include plot objects that have not
#' yet been rendered. The resulting representation isn't always
#' very pretty, though it is generally fairly readable. 
#' 
#' @name quarto_format
#' 
#' @aliases 
#' format.quarto_object
#' format.quarto_div
#' format.quarto_span
#' format.quarto_tabset
#' format.quarto_section
#' format.quarto_group
#' format.quarto_markdown
#' 
#' @examples
#' # formatted sections, spans and divs ----------------------------------
#' sec <- quarto_section("Header", level = 2L)
#' spn <- quarto_span("Content", class = "underline")
#' div <- quarto_div("Content", class = "content-margin")
#' 
#' format(sec)
#' 
#' format(spn)
#' 
#' format(div)
#' 
#' # formatted tabsets ---------------------------------------------------
#' tbs <- quarto_tabset(
#'   content = list(tab1 = 1:10, tab2 = "hello"),
#'   title = "Header",
#'   level = 2L
#' )
#' 
#' format(tbs)
#' 
#' # formatted groups and markdown ---------------------------------------
#' 
#' mkd <- quarto_markdown(list("- this is a", "- markdown list"), sep = "\n")
#' gps <- quarto_group(list(div, mkd))
#' 
#' format(mkd)
#' 
#' format(gps)
#' 
NULL

#' @rdname quarto_format
#' @exportS3Method base::format
format.quarto_section <- function(x, ...) {

  hashes <- paste(rep("#", x$level), collapse = "")
  header <- paste0("\n\n", hashes, " ", x$title, "\n\n")

  return(header)
}

#' @rdname quarto_format
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
  tabname <- purrr::map_chr(x$names, function(tt) {
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

    # for graphics objects (ggplot2/patchwork, base R recorded plots, grid
    # grobs, lattice/trellis objects, or anything tagged via
    # as_quarto_graphic()) we do no formatting at this stage. all we do
    # is wrap them safely within a quarto_* class, so that later on
    # when the print method is called, it is handled as a quarto obj.
    # wrapping in list() here is essential: quarto_plot() is itself a
    # list, so appending it with c() directly would flatten it into
    # `out` and silently strip its class (see PLAN.md).
    if (is_graphic(x$content[[i]])) {
      out <- c(out, list(quarto_plot(content = x$content[[i]])))
    
    # for everything else, call knit_print() now in order to produce 
    # character strings that can be passed directly to document with
    # cat(), but don't actually print them yet, just capture and store.
    # captured output may contain "<" / ">" from the default print
    # representation of arbitrary R objects (e.g. tibble's "<fct>"
    # column-type tags); escape these so they aren't parsed as HTML tags.
    } else {
      captured <- utils::capture.output(knitr::knit_print(x$content[[i]]))
      out <- c(out, pre_open)
      out <- c(out, protect_angle_brackets(captured))
      out <- c(out, pre_shut)
    }

  }
  out <- c(out, tabset_shut)

  return(out)
}

# divs and spans -------------------------------------------

#' @rdname quarto_format
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

#' @rdname quarto_format
#' @exportS3Method base::format
format.quarto_span <- function(x, ...) {

  span_content <- paste(unlist(x$content), sep = x$sep, collapse = x$sep)
  if (is.null(x$class) | !length(x$class)) x$class <- "quartose-null"
  css_info <- paste(".", x$class, sep = "", collapse = " ")
  span <- paste("[", span_content, "]{", css_info, "}", sep = "")

  return(span)
}

# groups of output -------------------------------------------

#' @rdname quarto_format
#' @exportS3Method base::format
format.quarto_markdown <- function(x, ...) {
  md <- format_elements(x$content)
  md <- paste(unlist(md), collapse = x$sep)
  return(md)
}

#' @rdname quarto_format
#' @exportS3Method base::format
format.quarto_group <- function(x, ...) {
  out <- format_elements(x$content)
  out <- purrr::flatten(out)
  return(out)
} 

# formatting helpers -----------------------------------------

format_elements <- function(x, ...) {
  purrr::map(x, function(cc) format(cc, ...))
}

# escape "<" and ">" in captured output so that literal text such as a
# tibble's "<fct>" column-type tag is not parsed as an (unknown) HTML tag
# by the quarto/pandoc markdown parser. Order matters: escape "&" first
# would be needed if we ever escape ampersands too, but for now we only
# protect the two characters that can form spurious tags.
protect_angle_brackets <- function(x) {
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

# render a graphics object recognized by is_graphic() (other than
# ggplot/patchwork, which already have their own knit_print rendering) to
# a temporary PNG file, and return its path. used by knit_print.quarto_plot()
render_graphic_png <- function(x, width = 7, height = 5, res = 96) {
  path <- tempfile(fileext = ".png")
  grDevices::png(path, width = width, height = height, units = "in", res = res)
  on.exit(grDevices::dev.off(), add = TRUE)
  draw_graphic(x)
  path
}

# draw a recognized graphics object to the currently active device
draw_graphic <- function(x) {
  if (inherits(x, "recordedplot")) {
    grDevices::replayPlot(x)
  } else if (inherits(x, "grob")) {
    grid::grid.newpage()
    grid::grid.draw(x)
  } else {
    # trellis objects, and anything tagged via as_quarto_graphic() that
    # isn't otherwise recognized, are drawn via their own print method.
    # this is a best-effort fallback: it assumes print(x) draws a plot to
    # the active device, which is true for many (but not all) S3 objects
    # that behave like graphics.
    print(x)
  }
}


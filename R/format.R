
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
#' are either strings or plot objects. For `quarto_div` objects, the
#' output is a string unless `content` includes a graphics object, in
#' which case it is a list whose elements are either strings or plot
#' objects, exactly as for `quarto_tabset`.
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
#' **Escaping policy.** `quarto_tabset()` accepts arbitrary R objects as
#' `content` and captures their default printed representation (via
#' `knitr::knit_print()`/`capture.output()`) to display inside each tab.
#' Most objects' `knit_print()`/`print()` methods write this representation
#' as a side effect (for example, an `lm` object's `Call:` summary, or a
#' data frame's default print), and that captured text may incidentally
#' contain `<` or `>` (for example, a tibble's `<fct>` column-type tag),
#' which would otherwise be parsed as an unrecognized HTML tag by
#' quarto/pandoc and silently dropped from the rendered document. To
#' prevent this, `format.quarto_tabset()` escapes `<` and `>` to
#' `&lt;`/`&gt;` in that captured text before it is written out, and wraps
#' it in `<pre>` tags. Some objects instead *return* their output marked
#' via `knitr::asis_output()` (class `"knit_asis"`) rather than printing
#' it — this includes `knitr::kable(format = "html")`, `flextable`
#' objects, `gt` tables, and most htmlwidget-like objects. For these,
#' `format.quarto_tabset()` detects the `"knit_asis"` class and emits the
#' raw markup unescaped and without a `<pre>` wrapper, so it is rendered
#' as intended (a table, widget, etc.) rather than displayed as literal
#' text. `quarto_div()` supports the same `"knit_asis"` passthrough for its
#' `content`, emitted unescaped for the same reason — but unlike
#' `quarto_tabset()`, it validates content at construction time, so objects
#' that neither print like ordinary R objects nor return `"knit_asis"`
#' output (e.g. a bare list, a model object, or a number) are rejected up
#' front rather than falling back to captured/escaped text. `quarto_span()`
#' content is restricted by validation to character vectors (never
#' arbitrary captured print output), and `quarto_markdown()` is explicitly
#' meant to carry raw markdown/HTML through untouched — neither of these
#' is escaped.
#' 
#' @name quarto_format
#' 
#' @aliases format.quarto_object format.quarto_div format.quarto_span format.quarto_tabset format.quarto_section format.quarto_group format.quarto_markdown
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
    
    # for everything else, capture knit_print() output via
    # knit_print_capture(): most objects write their output as a side
    # effect (e.g. lm's "Call: ..." summary), captured as plain text and
    # escaped/`<pre>`-wrapped below; some instead *return* asis-marked
    # HTML (e.g. knitr::kable(format = "html"), flextable), which is
    # emitted as raw markup instead. See knit_print_capture() for details.
    } else {
      kpc <- knit_print_capture(x$content[[i]])
      if (!is.null(kpc$raw)) {
        out <- c(out, kpc$raw)
      } else {
        out <- c(out, pre_open)
        out <- c(out, protect_angle_brackets(kpc$captured))
        out <- c(out, pre_shut)
      }
    }

  }
  out <- c(out, tabset_shut)

  return(out)
}

# divs and spans -------------------------------------------

#' @rdname quarto_format
#' @exportS3Method base::format
format.quarto_div <- function(x, ...) {

  has_graphic <- any(purrr::map_lgl(x$content, is_graphic))

  if (has_graphic) {
    # mirrors format.quarto_tabset(): graphics objects are wrapped in
    # quarto_plot() and kept as list elements (not pasted into a string)
    # so that knit_print.quarto_object() can render them later. wrapping
    # in list() is essential here for the same reason it is in
    # format.quarto_tabset() -- see the comment there.
    n <- length(x$content)
    div_body <- list()
    for (i in seq_len(n)) {
      el <- x$content[[i]]
      if (is_graphic(el)) {
        div_body <- c(div_body, list(quarto_plot(content = el)))
      } else {
        div_body <- c(div_body, format_div_element(el))
      }
      if (i < n) div_body <- c(div_body, x$sep)
    }
  } else {
    div_body <- purrr::map(x$content, format_div_element)
    div_body <- paste(unlist(div_body), collapse = x$sep)
  }

  if (is.null(x$class) | !length(x$class)) x$class <- "quartose-null"
  css_info <- paste(".", x$class, sep = "", collapse = " ")
  div_open <- paste("\n\n::: {", css_info, "}\n\n", sep = "", collapse = " ")
  div_shut <- "\n\n:::\n\n"

  # only when the div contains a graphic does format() return a list
  # (mixed strings and quarto_plot objects) instead of a single string;
  # this matches the documented contract in ?quarto_format
  if (has_graphic) {
    return(c(list(div_open), div_body, list(div_shut)))
  }

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

# Runs knitr::knit_print() on `x` exactly once, capturing both any output
# it writes as a side effect and its return value. Most objects (e.g. lm,
# data.frame) write their printed representation as a side effect via
# print()/cat(), returning something other than "knit_asis"-classed
# content; capturing that side-effect text is what the `captured` element
# is for. Some objects instead *return* their output marked via
# knitr::asis_output() (class "knit_asis") rather than printing it --
# this includes knitr::kable(format = "html"), flextable, gt, and most
# htmlwidget-like objects -- in which case `captured` is empty and the
# interesting content is unpacked into `raw` (a character vector, split
# on newlines, of the raw markup with the "knit_asis" wrapper removed).
# `raw` is NULL when `x` did not produce knit_asis-classed output.
# Shared by format.quarto_tabset() and format_div_element() so both
# handle these objects consistently.
knit_print_capture <- function(x) {
  captured <- utils::capture.output(kp <- knitr::knit_print(x))
  raw <- NULL
  if (inherits(kp, "knit_asis")) {
    raw <- unlist(strsplit(unclass(kp), "\n", fixed = TRUE))
  }
  list(captured = captured, raw = raw)
}

# Formats a single quarto_div() content element that is not itself a
# graphics object (graphics are handled by the caller, wrapped in
# quarto_plot() so rendering can be deferred). Character vectors and
# quarto objects are formatted via the format() generic, exactly as
# before. Anything else must have been validated by check_args_div() as
# producing knit_asis-classed content from knitr::knit_print() (e.g.
# knitr::kable(format = "html"), flextable, gt) -- for these, the raw
# markup is extracted via knit_print_capture() so it renders as intended
# rather than being coerced through format().
format_div_element <- function(el) {
  if (is_quarto(el) || rlang::is_character(el)) {
    return(format(el))
  }
  knit_print_capture(el)$raw
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


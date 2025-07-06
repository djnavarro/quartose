
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
#
# - all quarto_object classes share the same print method: in each 
#   case what it does is call format(), the methods for which are
#   idiosyncratic: different kinds of quarto_* objects will have
#   different formatting rules. however, the thing they have in 
#   common is that the formatting method will return a list, in 
#   which each element is either a character string, or it is a 
#   graphical object that can be printed into an as-is chunk 
#   with knit_print(). notice that the shared knit_print method
#   uses cat() to pass character strings to the document, but will
#   use knit_print() for graphics. it is implicitly assumed that
#   if character output needs knitr formatting before it is passed
#   into the document, any such formatting will be done inside the
#   format() method via capture.output(knit_print()) 


#' @title Print a quarto object
#' 
#' @description
#' Prints a quarto object. When calling `knitr::knit_print()`
#' on a quarto object, the relevant `format()` method is called
#' first, and the formatted version is printed to the document.
#' When calling `print()`, a summary of the object structure is
#' printed.
#' 
#' @param x A quarto object.
#' @param ... Other arguments (ignored).
#' 
#' @return 
#' `knitr::knit_print()` invisibly returns `NULL`; `print()` 
#' invisibly returns the quarto object itself.
#' 
#' @details
#' Additional details...
#' 
#' @name quarto_print
#' 
#' @aliases 
#' print.quarto_object
#' print.quarto_div
#' print.quarto_span
#' print.quarto_tabset
#' print.quarto_section
#' print.quarto_group
#' print.quarto_markdown
#' knit_print.quarto_object
#' knit_print.quarto_div
#' knit_print.quarto_span
#' knit_print.quarto_tabset
#' knit_print.quarto_section
#' knit_print.quarto_group
#' knit_print.quarto_markdown
#' 
#' @examples
#' # a quarto_section object
#' sec <- quarto_section("A level-two header", level = 2L)
#'  
#' # base::print() displays a summary of the object 
#' print(sec)
#' 
#' # knitr::knit_print() displays the rendered quarto syntax
#' knitr::knit_print(sec) 
#'
#' # a quarto_span object
#' spn <- quarto_span("This is underlined", class = "underline")
#' 
#' print(spn)
#' 
#' knitr::knit_print(spn)
#'  

#' @rdname quarto_print
#' @exportS3Method knitr::knit_print
knit_print.quarto_object <- function(x, ...) {
  fmt <- format(x, ...)
  purrr::walk(fmt, \(ff) {
    if (is.character(ff)) cat(ff, "\n")
    else knitr::knit_print(ff)
  })
  return(invisible(NULL))
}

#' @rdname quarto_print
#' @exportS3Method base::print
print.quarto_object <- function(x, ...) {
  cls <- class(x)[1]
  vals <- purrr::imap_chr(x, \(value, name) {
    if (name != "content" | rlang::is_character(value)) {
      return(paste(unname(as.character(value)), collapse = " "))
    }
    return(paste0("<", class(value)[1], ">"))
  })
  cli::cli({
    cli::cli_text("<", cls, ">");
    cli::cli_ul(items = paste0(names(x), ": ", vals))
  })
  return(invisible(x))
}


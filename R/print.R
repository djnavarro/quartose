
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

#' @exportS3Method knitr::knit_print
knit_print.quarto_object <- function(x, ...) {
  fmt <- format(x, ...)
  purrr::walk(fmt, \(ff) {
    if (is.character(ff)) cat(ff, "\n")
    else knitr::knit_print(ff)
  })
}

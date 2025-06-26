#' @title Insert output sections
#' @description
#' Opens and closes an output section in a quarto document
#' 
#' @name output
NULL

#' @export
#' @rdname output
open_output <- function() {
  cat("<pre>")
}

#' @export
#' @rdname output
close_output <- function() {
  cat("</pre>")
}

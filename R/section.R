
#' @title Insert section header
#' @description
#' Places a section header in a quarto document
#' 
#' @param title Header title
#' @param level Numeric specfying the header level 
#' @export
section_header <- function(title, level) {
  hashes <- paste(rep("#", level), collapse = "")
  knitr::asis_output(paste0("\n\n", hashes, " ", title, "\n\n"))
}

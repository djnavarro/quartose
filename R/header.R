
#' @title Insert section header
#' @description
#' Places a section header in a quarto document
#' 
#' @param .title Header title
#' @param .level Numeric specfying the header level 
#' @export
header <- function(.title, .level) {
  hashes <- paste(rep("#", .level), collapse = "")
  header <- paste0("\n\n", hashes, " ", .title, "\n\n")
  cat(header)
}

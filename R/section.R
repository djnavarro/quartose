
section_header <- function(..., level) {

  cat("\n\n")
  cat(paste(rep("#", level), collapse = ""))
  cat(" ")
  cat(...)
  cat("\n\n")

}

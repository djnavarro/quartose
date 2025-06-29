
format_elements <- function(x, ...) {
  purrr::map(x, \(cc) format(cc, ...))
}

is_ggplot <- function(x) {
  inherits(x, "ggplot")  # TODO: maybe use the ggplot2 version
}

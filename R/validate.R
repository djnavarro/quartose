
# tabs and sections -------------------------------------------

check_args_section <- function(title, level) {
  if (!rlang::is_integerish(level, n = 1)) rlang::abort("level must be a single integer")
  if (!rlang::is_character(title, n = 1)) rlang::abort("title must be a single character string")
  if (level < 1L | level > 6L) rlang::abort("level must be an integer between 1 and 6")
}

check_args_tabset <- function(content, level, title, names) { 
  if (!rlang::is_integerish(level, n = 1)) rlang::abort("level must be a single integer")
  if (level < 1L | level > 6L) rlang::abort("level must be an integer between 1 and 6")
  if (!rlang::is_null(title)) {
    if (!rlang::is_character(title, n = 1)) rlang::abort("title must be a single character string") 
  }
  if (!rlang::is_list(content)) rlang::abort("content must be a list")
  if (rlang::is_null(names)) {
    # the only way `names` can still be NULL here is if the caller didn't
    # supply `names` explicitly *and* `content` has no names of its own
    # (quarto_tabset() falls back to `names(content)` when `names = NULL`)
    rlang::abort(
      "content has no names, and `names` was not supplied. Either provide `names` explicitly, or name the elements of `content` (e.g. `content = list(a = ..., b = ...)`)."
    )
  }
  if (!rlang::is_character(names)) rlang::abort("names must be a character vector")
  if (length(content) != length(names)) rlang::abort("content and names must have the same length")
}

# divs and spans -------------------------------------------

check_args_div <- function(content, class, sep) {
  if (!rlang::is_character(sep, n = 1)) rlang::abort("sep must be a single character string") 
    if (!rlang::is_null(class)) {
      if (!rlang::is_character(class)) rlang::abort("class must be a character vector")
      if (any(rlang::are_na(class) | nchar(class) == 0)) {
        rlang::warn("class contains missing values or empty strings")
      }
    }
  # an empty div (no content supplied) is a deliberate edge case: quarto_div()
  # wraps a bare `NULL` into `list(NULL)` before this check runs, so it must
  # be special-cased rather than rejected as "not character/quarto_object"
  is_empty_div <- length(content) == 0 || (length(content) == 1 && rlang::is_null(content[[1]]))
  if (!is_empty_div) {
    is_valid <- purrr::map_lgl(content, function(x) rlang::is_character(x) || is_quarto(x) || is_graphic(x))
    if (!all(is_valid)) {
      rlang::abort("all elements of content must be character vectors, quarto objects, or graphics objects recognized by is_graphic() (see as_quarto_graphic())")
    }
  }
}

check_args_span <- function(content, class, sep) {
  if (!rlang::is_character(content)) rlang::abort("content must be a character vector")
  if (!rlang::is_character(sep, n = 1)) rlang::abort("sep must be a single character string") 
  if (!rlang::is_null(class)) {
    if (!rlang::is_character(class)) rlang::abort("class must be a character vector")
    if (any(rlang::are_na(class) | nchar(class) == 0)) {
      rlang::warn("class contains missing values or empty strings")
    }
  }
}

# groups of output -------------------------------------------

check_args_group <- function(content, sep) {
  is_q <- purrr::map_lgl(content, is_quarto)
  if (!all(is_q)) rlang::abort("all elements of content must all be quarto objects")
  if (!rlang::is_character(sep, n = 1)) rlang::abort("sep must be a single character string")
}

check_args_markdown <- function(content, sep) {
  is_c <- purrr::map_lgl(content, rlang::is_character)
  if (!all(is_c)) rlang::abort("all elements of content must all be character vectors")
  if (!rlang::is_character(sep, n = 1)) rlang::abort("sep must be a single character string")
}

# class checkers ---------------------------------------------

is_quarto <- function(x) {
  inherits(x, "quarto_object")
}

is_ggplot <- function(x) {
  inherits(x, "ggplot")  
}

# patchwork objects subclass ggplot, so is_ggplot() already recognizes them.
# recordedplot: produced by grDevices::recordPlot() (base R graphics)
is_recorded_plot <- function(x) {
  inherits(x, "recordedplot")
}

# grob: grid graphical objects (the foundation lattice/ggplot2 are built on)
is_grob <- function(x) {
  inherits(x, "grob")
}

# trellis: lattice package plot objects
is_trellis <- function(x) {
  inherits(x, "trellis")
}

# escape hatch: as_quarto_graphic() tags an arbitrary object with this class
# so it is treated as a graphic even if none of the predicates above
# recognize it (e.g. a plotting object from a package quartose doesn't
# know about)
is_tagged_graphic <- function(x) {
  inherits(x, "quartose_graphic")
}

# dispatch table used by is_graphic(): add new predicates here to extend
# auto-detection to further graphics classes
graphic_predicates <- list(
  is_tagged_graphic,
  is_ggplot,
  is_recorded_plot,
  is_grob,
  is_trellis
)

#' Is this object recognized as a graphic?
#'
#' Generalizes `is_ggplot()` to a small dispatch table covering ggplot2
#' (and patchwork, which subclasses ggplot2), base R recorded plots
#' (`grDevices::recordPlot()`), grid grobs, and lattice/trellis objects,
#' plus anything explicitly tagged via `as_quarto_graphic()`.
#' @noRd
is_graphic <- function(x) {
  any(vapply(graphic_predicates, function(pred) pred(x), logical(1)))
}

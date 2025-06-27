
# tabs and sections -------------------------------------------

#' @title Quarto section header
#' @param .title, Section title
#' @param .level, Header level
#' @export
quarto_section <- function(.title, .level) {
  structure(
    rlang::list2(
      title = .title,
      level = .level
    ),
    class = "quarto_section"
  )
}

#' @title Quarto tabset
#' @param ..., Tab title and content as name-value pairs
#' @param .level, Header level associated with the tabs
#' @export
quarto_tabset <- function(..., .level = 3L) {
  .content <- rlang::list2(...)
  structure(
    rlang::list2(
      content = .content,
      title = names(.content),
      level = .level
    ),
    class = "quarto_tabset"
  )
}

#' @title Quarto tabset with section header
#' @param .content, Tab title and content as named list
#' @param .title, Section title
#' @param .level, Header level associated with the section header
#' @export
quarto_tabsec <- function(.content, .title, .level = 3L) {
  structure(
    rlang::list2(
      content = .content,
      title = .title,
      subtitle = names(.content),
      level = .level
    ),
    class = "quarto_tabsec"
  )
}

# groups of output -------------------------------------------

#' @title Quarto groups
#' @param ..., Content
#' @param .sep, Separator
#' @name groups

#' @rdname groups
#' @export
quarto_output <- function(...) {
  structure(
    rlang::list2(
      content = rlang::list2(...)
    ),
    class = "quarto_output"
  )
}

#' @rdname groups
#' @export
quarto_markdown <- function(..., .sep = "") {
  structure(
    rlang::list2(
      content = rlang::list2(...),
      sep = .sep,
    ),
    class = "quarto_markdown"
  )
}

#' @rdname groups
#' @export
quarto_paragraph <- function(..., .sep = "") {
  structure(
    rlang::list2(
      content = rlang::list2(...),
      sep = .sep,
    ),
    class = "quarto_paragraph"
  )
}

# divs and spans -------------------------------------------

#' @title Quarto divs
#' @param ..., Content to be contained in the div
#' @param .class, CSS class (or vector of classes) for the div
#' @param .sep, Separator
#' @export
quarto_div <- function(..., .class, .sep = "") {
  structure(
    rlang::list2(
      content = rlang::list2(...),
      class = .class,
      sep = .sep,
    ),
    class = "quarto_div"
  )
}

#' @title Quarto span
#' @param .content, Content to be contained in the span
#' @param .class, CSS class (or vector of classes) for the span
#' @export
quarto_span <- function(.content, .class = NULL) {
  structure(
    rlang::list2(
      content = .content,
      class = .class
    ),
    class = "quarto_span"
  )
}

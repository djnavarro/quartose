
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
    class = c("quarto_section", "quarto_object")
  )
}

#' @title Quarto tabset
#' @param .content, Tab title and content as a named list
#' @param .level, Header level associated with the tabs
#' @export
quarto_tabset <- function(.content , .level = 3L) {
  structure(
    rlang::list2(
      content = .content,
      title = names(.content),
      level = .level
    ),
    class = c("quarto_tabset", "quarto_object")
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
    class = c("quarto_tabsec", "quarto_object")
  )
}

# groups of output -------------------------------------------

#' @title Quarto groups
#' @param ..., Objects to be rendered as a group
#' @param .sep, Separator
#' @name quarto_group

#' @rdname quarto_group
#' @export
quarto_output <- function(...) {
  structure(
    rlang::list2(
      content = rlang::list2(...)
    ),
    class = c("quarto_output", "quarto_object")
  )
}

#' @rdname quarto_group
#' @export
quarto_markdown <- function(..., .sep = "") {
  structure(
    rlang::list2(
      content = rlang::list2(...),
      sep = .sep,
    ),
    class = c("quarto_markdown", "quarto_object")
  )
}

#' @rdname quarto_group
#' @export
quarto_paragraph <- function(..., .sep = "") {
  structure(
    rlang::list2(
      content = rlang::list2(...),
      sep = .sep,
    ),
    class = c("quarto_paragraph", "quarto_object")
  )
}

# divs and spans -------------------------------------------

#' @title Quarto divs and spans
#' @param .content, Content to be contained in the div/span
#' @param .class, CSS class (or vector of classes) for the div/span
#' @param .sep, Separator used when merging content
#' @name quarto_div

#' @rdname quarto_div
#' @export
quarto_div <- function(.content, .class = NULL, .sep = "") {
  structure(
    rlang::list2(
      content = .content,
      class = .class,
      sep = .sep,
    ),
    class = c("quarto_div", "quarto_object")
  )
}

#' @rdname quarto_div
#' @export
quarto_span <- function(.content, .class = NULL, .sep = "") {
  structure(
    rlang::list2(
      content = .content,
      class = .class,
      sep = .sep,
    ),
    class = c("quarto_span", "quarto_object")
  )
}

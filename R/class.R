
# tabs and sections -------------------------------------------

#' @title Quarto section header
#' @param title, Section title
#' @param level, Header level
#' 
#' @examples
#' quarto_section("A level-two header", level = 2L)
#' quarto_section("A level-three header", level = 3L)
#' 
#' @export
quarto_section <- function(title, level) {
  check_args_section(title, level)
  structure(
    rlang::list2(
      title = title,
      level = level
    ),
    class = c("quarto_section", "quarto_object")
  )
}

#' @title Quarto tabset with section header
#' @param content, Tab content as a list
#' @param level, Header level
#' @param title, Section title (optional)
#' @param names, Names for tabs (defaults to names(content))
#' 
#' @examples
#' quarto_tabset(list(tab1 = 1:10, tab2 = "hello"), level = 3L) 
#' 
#' @export
quarto_tabset <- function(content, level, title = NULL, names = NULL) {

  if (rlang::is_null(names)) names <- names(content)
  check_args_tabset(content, level, title, names)
  
  structure(
    rlang::list2(
      content = content,
      title = title,
      names = names,
      level = level
    ),
    class = c("quarto_tabset", "quarto_object")
  )
}

# divs and spans -------------------------------------------

#' @title Quarto divs and spans
#' @param content, Content to be contained in the div/span
#' @param class, CSS class (or vector of classes) for the div/span
#' @param sep, Separator used when merging content
#' @name quarto_div
#' 
#' @examples
#' quarto_span("This is underlined", class = "underline")
#' quarto_div("This is a callout note", class = "callout-note")
#' 
NULL

#' @rdname quarto_div
#' @export
quarto_div <- function(content, class = NULL, sep = "") {
  if (!rlang::is_bare_list(content)) content = list(content)
  check_args_div(content, class, sep)
  structure(
    rlang::list2(
      content = content,
      class = class,
      sep = sep,
    ),
    class = c("quarto_div", "quarto_object")
  )
}

#' @rdname quarto_div
#' @export
quarto_span <- function(content, class = NULL, sep = "") {
  check_args_span(content, class, sep)
  structure(
    rlang::list2(
      content = content,
      class = class,
      sep = sep,
    ),
    class = c("quarto_span", "quarto_object")
  )
}

# groups of output -------------------------------------------

#' @title Quarto groups
#' @param content, A list of objects
#' @param sep, Separator
#' @name quarto_group
#' @examples
#' quarto_markdown(list("- a markdown", "- list"), sep = "\n")
#' quarto_group(list(
#'   quarto_div("This is a callout note", class = "callout-note"),
#'   quarto_div("This is a callout tip", class = "callout-tip")
#' ))


#' @rdname quarto_group
#' @export
quarto_group <- function(content, sep = "") {
  check_args_group(content, sep)
  structure(
    rlang::list2(
      content = content,
      sep = sep
    ),
    class = c("quarto_group", "quarto_object")
  )
}

#' @rdname quarto_group
#' @export
quarto_markdown <- function(content, sep = "") {
  check_args_markdown(content, sep = sep)
  structure(
    rlang::list2(
      content = content,
      sep = sep,
    ),
    class = c("quarto_markdown", "quarto_object")
  )
}


# plots -------------------------------------------------------

quarto_plot <- function(content) {
  structure(
    rlang::list2(
      content = content,
    ),
    class = c("quarto_plot", "quarto_object")
  )
}


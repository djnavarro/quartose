
#' @title Dynamically generate quarto syntax
#' 
#' @description
#' Define quarto objects for insertion into a document. 
#' Intended to be used inside a quarto document, within a knitr 
#' code chunk with the `results: asis` option set.
#'   
#' @param content Tab content as a list.
#' @param title Section title, a character string.
#' @param level Header level, an integer between 1 and 6.
#' @param names Names for tabs (defaults to names(content)).
#' @param class CSS class (or vector of classes) to be supplied.
#' @param sep Separator used when merging content.
#'  
#' @return 
#' These functions always return an object with parent S3 class 
#' "quarto_object", in addition to a specific S3 class corresponding
#' to the function (e.g., `quarto_section()` objects also possess the
#' "quarto_section" class). When printed, the object is
#' rendered to a character vector of length 1 containing the quarto 
#' syntax that inserts an HTML section header into the document.
#' 
#' @details
#' Additional details...
#' 
#' @name quarto_object
#' @aliases 
#' quarto_div
#' quarto_span
#' quarto_tabset
#' quarto_section
#' quarto_group
#' quarto_markdown
#' 
#' @examples
#' sec <- quarto_section("A level-two header", level = 2L)
#'  
#' # Use knitr::knit_print() to see the rendered quarto syntax
#' knitr::knit_print(sec)
#' 
#' # Use base::print() to see the structure of the object
#' print(sec)
#'
#' spn <- quarto_span("This is underlined", class = "underline")
#' knitr::knit_print(spn)
#'  
#' div <- quarto_div("This is a callout note", class = "callout-note")
#' knitr::knit_print(div)
#' 
#' tbs <- quarto_tabset(list(tab1 = 1:10, tab2 = "hello"), level = 3L)
#' knitr::knit_print(tbs)
#' 
#' mkd <- quarto_markdown(list("- a markdown", "- list"), sep = "\n")
#' knitr::knit_print(mkd)
#' 
#' grp <- quarto_group(list(
#'   quarto_div("This is a callout note", class = "callout-note"),
#'   quarto_div("This is a callout tip", class = "callout-tip")
#' ))
#' knitr::knit_print(grp)
NULL

# tabs and sections -------------------------------------------

#' @rdname quarto_object
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

#' @rdname quarto_object
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

#' @rdname quarto_object
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

#' @rdname quarto_object
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

#' @rdname quarto_object
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

#' @rdname quarto_object
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


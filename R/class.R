
#' @title Dynamically generate quarto syntax
#' 
#' @description
#' Define quarto objects for insertion into a document. 
#' Intended to be used inside a quarto document, within a knitr 
#' code chunk with the `results: asis` option set.
#'   
#' @param content 
#' List or character vector containing content to be included within 
#' the quarto object. The expected format of the `content` argument 
#' differs slightly depending on which function is used. See the 
#' "details" section for more information.
#' 
#' @param title 
#' Character string specifying the text to use as a section title. 
#' For `quarto_section()` this is a required argument. For `quarto_tabset()` 
#' it is permitted to use `title = NULL`, in which case the tabset will 
#' be printed without a section header above it. This is the default 
#' behaviour for tabsets.
#' 
#' @param level 
#' Numeric header level applied to section title or tabset names. 
#' The `level` argument must be a whole number between 1 and 6. Only 
#' relevant to quarto objects that produce section headings, specifically 
#' `quarto_section()` and `quarto_tabset()`.
#' 
#' @param names 
#' Character vector of names to be applied to the tabs in a tabset. Only 
#' relevant to  `quarto_tabset()`. If `names = NULL`, the names will be 
#' taken from the names of the `content` argument.
#' 
#' @param class 
#' Character vector specifying CSS classes to be applied to the content. 
#' Only relevant to `quarto_div()` and `quarto_span()`. Defaults to 
#' `class = NULL`, in which case the formatted text written to the document 
#' will have a dummy CSS class "quartose-null" applied.
#' 
#' @param sep 
#' Character string specifying the separator to be used when merging content
#' for printing to the document. Defaults to `sep = ""` for all functions.
#'  
#' @return 
#' These functions always return an object with parent S3 class 
#' "quarto_object", in addition to a specific S3 class corresponding
#' to the function. For example, `quarto_section()` objects also possess the
#' "quarto_section" class.
#' 
#' @details
#' The purpose of these functions is to allow the user to dynamically 
#' generate quarto syntax from R. When used within a quarto document they
#' allow the user to generate callouts, margin text, tabsets, section 
#' headers, and other kinds of quarto output. At the current state of 
#' development the functionality is somewhat limited, discussed below.
#' 
#' The `quarto_*()` functions supplied by the quartose package have a common
#' design: argument values supplied by the user are stored internally as a 
#' list, with only a minimum of processing done at the time that the function
#' is called. The object is assigned to two S3 classes, the "quarto_object"
#' shared by all objects, and a specific class associated with the calling 
#' function. These objects can be inspected and manipulated programmatically
#' like any other R objects prior to printing.
#' 
#' When creating a quarto object, note that most `quarto_*()` functions take 
#' a `content` argument, which differs slightly depending on the context:
#' 
#' - For `quarto_section()` there is no `content`` argument: section headers 
#'   have titles, but they do not contain content.
#' - For `quarto_span()` the `content`` argument *must* be a character vector, 
#'   not a list.
#' - For `quarto_div()` the `content`` argument is permitted to be a character
#'   vector or a list, but it will always be stored internally as a list. 
#'   If the input is a list, it can contain other quarto objects. The 
#'   intended use for this is a div that contains several spans, but it is
#'   not limited to this use case. At present, `quarto_div()` cannot handle 
#'   plot objects, but functionality may be extended to permit this in future.
#' - For `quarto_tabset()` the `content` argument *must* be a list. The list 
#'   elements can be any printable R object: each element of the list will
#'   appear in its own tab. At present the support for graphics objects is
#'   limited: ggplot2 objects are captured and will only be rendered when
#'   `knitr::knit_print()` is called. No attempt is made (as yet!) to support
#'   other kinds of graphic objecvts, and if these are passed via the `content`
#'   argument the function will likely fail.
#' - For `quarto_markdown()` the `content` argument may be a character vector
#'   or a list of character vectors. The function will throw an error if other
#'   kinds of objects are passed via `content`.
#' - For `quarto_group()` the `content` argument *must* be a list, and all 
#'   elements of the list must be quarto objects. The intended use of this 
#'   function is simply to collect several quarto objects into a single group
#'   that will be printed all at the same time rather than sequentially.
#' 
#' Creating a quarto object only defines the data structure, it does not 
#' perform any formatting. Similarly, if the object is printed using 
#' `print()`, no formatting will be applied. A brief summary of the 
#' data structure will be printed to the console, no more. However, when
#' `knitr::knit_print()` is called, the quarto object is first passed to
#' the relevant `format()` method, which is responsible for constructing 
#' the appropriate quarto syntax. Calling `format()` will return a
#' character vector or a list. If it returns a list all elements will 
#' either be character strings with the appropriate quarto syntax, or a
#' plot object that has not yet been rendered. After formatting is applied
#' the `knitr::knit_print()` method will pass the strings (or plots) to 
#' the document. For more detail on the formatting and printing methods 
#' see `knit_print.quarto_object()` and `format.quarto_object()`.
#' 
#' @name quarto_object
#' 
#' @aliases 
#' quarto_div
#' quarto_span
#' quarto_tabset
#' quarto_section
#' quarto_group
#' quarto_markdown
#' 
#' @examples
#' # quarto_section ------------------------------------------------------
#' 
#' sec <- quarto_section("A level-two header", level = 2L)
#' 
#' # quarto objects have two classes, a general purpose class shared by 
#' # all quarto objects, and a class specific to the function
#' class(sec) 
#'  
#' # base::print() displays an abstract summary of the object 
#' print(sec)
#' 
#' # knitr::knit_print() produces the rendered quarto syntax
#' knitr::knit_print(sec)
#' 
#' # quarto_span ---------------------------------------------------------
#'
#' spn1 <- quarto_span("This is plain text")
#' spn2 <- quarto_span("This is underlined text", class = "underline")
#' 
#' print(spn1)
#' 
#' print(spn2)
#' 
#' knitr::knit_print(spn1)
#' 
#' knitr::knit_print(spn2)
#' 
#' # quarto_div ----------------------------------------------------------
#' 
#' # quarto_div objects are flexible: they can take a character vector as
#' # the content argument, but can also take lists of other objects; note
#' # that internally the content is always represented as a list
#' div1 <- quarto_div("This is a callout note", class = "callout-note")
#' div2 <- quarto_div(
#'   content = list(
#'     quarto_span(content = "You can wrap multiple spans in a div so that"),
#'     quarto_span(content = "some text is highlighted", class = "mark"),
#'     quarto_span(content = "and some is underlined", class = "underline")
#'   ),
#'   class = c("column-margin", "callout-tip"),
#'   sep = " "
#' )
#' 
#' print(div1)
#' 
#' print(div2)
#' 
#' knitr::knit_print(div1)
#' 
#' knitr::knit_print(div2)
#' 
#' # quarto_tabset -------------------------------------------------------
#' 
#' tbs <- quarto_tabset(list(tab1 = 1:10, tab2 = "hello"), level = 3L)
#' 
#' print(tbs)
#' 
#' knitr::knit_print(tbs)
#' 
#' # quarto_markdown -----------------------------------------------------
#' 
#' mkd <- quarto_markdown(list("- a markdown", "- list"), sep = "\n")
#' 
#' print(mkd)
#' 
#' knitr::knit_print(mkd)
#' 
#' # quarto_group --------------------------------------------------------
#' 
#' grp <- quarto_group(list(
#'   quarto_div("This is a callout note", class = "callout-note"),
#'   quarto_div("This is a callout tip", class = "callout-tip")
#' ))
#' 
#' print(grp)
#' 
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


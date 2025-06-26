#' @title Insert a tabset
#' @description
#' Opens and closes a tabs section in a quarto document
#' 
#' @param ..., Contents of each tab as name-value pairs
#' @param .level, Depth level for the implied section headers
#' 
#' @export
tabs <- function(..., .level = 3L) {
  d <- rlang::list2()
  d$content <- rlang::list2(...)
  d$title <- names(d$content)
  d$level <- .level
  structure(d, class = "quartose_tabs")
}


#' @exportS3Method knitr::knit_print
knit_print.quartose_tabs <- function(x, ...) {

  # open tabset
  cat("\n\n::: {.panel-tabset}\n\n")
  for(i in seq_along(x$content)) {

    # create tab with section header
    hashes <- paste(rep("#", x$level), collapse = "")
    header <- paste0("\n\n", hashes, " ", x$title[i], "\n\n")
    cat(header)

    # output
    if (ggplot2::is_ggplot(x$content[[i]])) {
      knitr::knit_print(x$content[[i]])
    } else {
      cat("<pre>")
      knitr::knit_print(x$content[[i]])
      cat("</pre>")
    }

  }

  # close tabset
  cat("\n\n::: \n\n")
}



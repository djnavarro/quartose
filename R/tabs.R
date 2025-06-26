#' @title Insert tabbed sections
#' @description
#' Opens and closes a tabs section in a quarto document
#' 
#' @name tabs
NULL

#' @export
#' @rdname tabs
open_tabs <- function() {
  cat("\n\n::: {.panel-tabset}\n\n")
}

#' @export
#' @rdname tabs
close_tabs <- function() {
  cat("\n\n::: \n\n")
}

#' @rdname tabs
#' @export
tabs <- function(..., .show_code = FALSE, .level = 3L) {
  d <- rlang::list2()
  d$content <- rlang::list2(...)
  d$code <- rlang::enquos(...)
  d$title <- names(d$content)
  d$show_code <- .show_code
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

    # show code if asked; experimental, do not use...
    if (x$show_code) {
      code_block <- paste0(
        '<div class="sourceCode cell-code"><pre class="sourceCode r code-with-copy"><code class="sourceCode r"><span>',
        rlang::as_label(x$code[[i]]),
        '</span></pre></code></div>'
      )
      cat(code_block)
    }

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



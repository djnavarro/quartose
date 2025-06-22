#' @title Insert tabbed sections
#' @description
#' Opens and closes a tabs section in a quarto document
#' 
#' @name tabs
NULL

open_tabs <- function() {
  knitr::asis_output("\n\n::: {.panel-tabset}\n\n")
}

close_tabs <- function() {
  knitr::asis_output("\n\n::: \n\n")
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

  n <- length(x$content)
  hashes <- paste(rep("#", x$level), collapse = "")
  headers <- paste0("\n\n", hashes, " ", x$title, "\n\n")

  out_list <- list()
  ind <- 1L

  # open tabs
  out_list[[ind]] <- literal("\n\n::: {.panel-tabset}\n\n")
  ind <- ind + 1L

  for(tab in 1:n) {
    
    # add header
    out_list[[ind]] <- literal(headers[tab])
    ind <- ind + 1L
    
    # experimental, do not use...
    if (x$show_code) {

      code_block <- paste0(
        '<div class="sourceCode cell-code"><pre class="sourceCode r code-with-copy"><code class="sourceCode r"><span>',
        rlang::as_label(x$code[[tab]]),
        '</span></pre></code></div>'
      )
      out_list[[ind]] <- literal(code_block)
      ind <- ind + 1L

    }

    # open <pre> tag for output
    out_list[[ind]] <- literal("<pre>")
    ind <- ind + 1L

    # the output content
    out_list[[ind]] <- x$content[[tab]] 
    ind <- ind + 1L

    # close </pre> tag for output
    out_list[[ind]] <- literal("</pre>")
    ind <- ind + 1L

  }

  # close tabs
  out_list[[ind]] <- literal("\n\n::: \n\n")

  out <- purrr::map_chr(
    out_list,
    \(x) {
      paste(capture.output(
        if (inherits(x, "quartose_literal")) {
          cat(x, "\n", sep = "") 
        } else {
          knitr::knit_print(x)
        }
      ), collapse = "\n")
    }
  )
  knitr::asis_output(out)
}


literal <- function(x) {
  class(x) <- "quartose_literal"
  x
}

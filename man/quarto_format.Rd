% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format.R
\name{quarto_format}
\alias{quarto_format}
\alias{format.quarto_object}
\alias{format.quarto_div}
\alias{format.quarto_span}
\alias{format.quarto_tabset}
\alias{format.quarto_section}
\alias{format.quarto_group}
\alias{format.quarto_markdown}
\title{Format a quarto object}
\usage{
\method{format}{quarto_section}(x, ...)

\method{format}{quarto_tabset}(x, ...)

\method{format}{quarto_div}(x, ...)

\method{format}{quarto_span}(x, ...)

\method{format}{quarto_markdown}(x, ...)

\method{format}{quarto_group}(x, ...)
}
\arguments{
\item{x}{A quarto object.}

\item{...}{Other arguments (ignored).}
}
\value{
A formatted quarto object. For \code{quarto_section}, \code{quarto_span},
and \code{quarto_markdown} objects, the formatted output is always a
string (character vector of length 1). For \code{quarto_tabset} and
\code{quarto_group} objects, the output is always a list whose elements
are either strings or plot objects. For \code{quarto_div} objects,
the output is currently a string, but this may change to list
output in future if divs are permitted to contain plots.
}
\description{
Creates a formatted representation of a quarto object in a
form suitable for printing. When calling \code{knitr::knit_print()}
on a quarto object, the relevant \code{format()} method is called
first, and the formatted version is printed to the document.
Note that the base \code{print()} method for quarto objects does
not call \code{format()}.
}
\details{
The intent behind the \code{format()} methods for quarto objects
is to create a ready-to-print representation of that is almost
identical to what will be printed into the quarto document
when \code{knitr::knit_print()} is called. Because of this, the
formatted version of a quarto object is a string or a list of
strings, but it may also include plot objects that have not
yet been rendered. The resulting representation isn't always
very pretty, though it is generally fairly readable.
}
\examples{
# formatted sections, spans and divs ----------------------------------
sec <- quarto_section("Header", level = 2L)
spn <- quarto_span("Content", class = "underline")
div <- quarto_div("Content", class = "content-margin")

format(sec)

format(spn)

format(div)

# formatted tabsets ---------------------------------------------------
tbs <- quarto_tabset(
  content = list(tab1 = 1:10, tab2 = "hello"),
  title = "Header",
  level = 2L
)

format(tbs)

# formatted groups and markdown ---------------------------------------

mkd <- quarto_markdown(list("- this is a", "- markdown list"), sep = "\n")
gps <- quarto_group(list(div, mkd))

format(mkd)

format(gps)

}

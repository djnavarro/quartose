% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class.R
\name{quarto_group}
\alias{quarto_group}
\alias{quarto_markdown}
\title{Quarto groups}
\usage{
quarto_group(content, sep = "")

quarto_markdown(content, sep = "")
}
\arguments{
\item{content, }{A list of objects}

\item{sep, }{Separator}
}
\description{
Quarto groups
}
\examples{
mkd <- quarto_markdown(list("- a markdown", "- list"), sep = "\n")
grp <- quarto_group(list(
  quarto_div("This is a callout note", class = "callout-note"),
  quarto_div("This is a callout tip", class = "callout-tip")
))

knitr::knit_print(mkd)
knitr::knit_print(grp)
}



# print methods for quarto_objects tend to be "asis", which
# is useful when applied at the top level, but needs to be
# stripped if we want to capture and manipulate raw output
strip_asis <- function(x) {
  if (inherits(x, "knit_asis")) return(c(unclass(x)))
  x
}


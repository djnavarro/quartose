
# tabs and sections -------------------------------------------

check_args_section <- function(.title, .level) {
  if (!rlang::is_integerish(.level, n = 1)) rlang::abort(".level must be a single integer")
  if (!rlang::is_character(.title, n = 1)) rlang::abort(".title must be a single character string")
  if (.level < 1L | .level > 6L) rlang::abort(".level must be an integer between 1 and 6")
}

check_args_tabset <- function(.content, .level, .title, .names) { 
  if (!rlang::is_integerish(.level, n = 1)) rlang::abort(".level must be a single integer")
  if (.level < 1L | .level > 6L) rlang::abort(".level must be an integer between 1 and 6")
  if (!rlang::is_null(.title)) {
    if (!rlang::is_character(.title, n = 1)) rlang::abort(".title must be a single character string") 
  }
  if (!rlang::is_list(.content)) rlang::abort(".content must be a list")
  if (!rlang::is_character(.names)) rlang::abort(".names must be a character vector")
  if (length(.content) != length(.names)) rlang::abort(".content and .names must have the same length")
}

# divs and spans -------------------------------------------

check_args_div <- function(.content, .class, .sep) {
  if (!rlang::is_character(.sep, n = 1)) rlang::abort(".sep must be a single character string") 
    if (!rlang::is_null(.class)) {
      if (!rlang::is_character(.class)) rlang::abort(".class must be a character vector")
      if (any(rlang::are_na(.class) | nchar(.class) == 0)) {
        rlang::warn(".class contains missing values or empty strings")
      }
    }
  }

check_args_span <- function(.content, .class, .sep) {
  if (!rlang::is_character(.content)) rlang::abort(".content must be a character vector")
  if (!rlang::is_character(.sep, n = 1)) rlang::abort(".sep must be a single character string") 
  if (!rlang::is_null(.class)) {
    if (!rlang::is_character(.class)) rlang::abort(".class must be a character vector")
    if (any(rlang::are_na(.class) | nchar(.class) == 0)) {
      rlang::warn(".class contains missing values or empty strings")
    }
  }
}

# groups of output -------------------------------------------

check_args_group <- function(.content, .sep) {
  is_q <- purrr::map_lgl(.content, is_quarto)
  if (!all(is_q)) rlang::abort("all elements of .content must all be quarto objects")
}

check_args_markdown <- function(..., .sep) {
  args <- rlang::list2(...)
  is_c <- purrr::map_lgl(args, rlang::is_character)
  if (!all(is_c)) rlang::abort("objects passed by ... must all be character vectors")
  if (!rlang::is_character(.sep, n = 1)) rlang::abort(".sep must be a single character string") 
}

# class checkers ---------------------------------------------

is_quarto <- function(x) {
  inherits(x, "quarto_object")
}

is_ggplot <- function(x) {
  inherits(x, "ggplot")  
}

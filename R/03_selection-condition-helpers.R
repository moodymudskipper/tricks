#' Selection Focused Condition Helpers
#'
#' These helpers are meant to be used on the left hand side of the trick specification,
#' to filter which tricks the addin should display.
#'
#' @param txt A character object
#' @param n number of lines
#' @param multi_ok Are multiple calls eligible ?
#' @param single_ok Are single calls eligible ?
#' @param symbol_ok Are symbols eligible ?
#' @param litteral_ok Are literal strings or numbers eligible ?
#' @param reserved_ok Are reserved words eligible ?
#' @param simple_only if `TRUE`, only symbols and calls to `::`, `:::`, `[`,
#'   `[[` or `$` are considered. It should be used on condition calls to make sure
#'   we never evaluate a call that would have side effects.
#' @param class class used to check inheritance
#' @inheritParams current_selection
#' @export
#' @name selection-condition-helpers
selection_is_empty <- function(target = c("default", "lines", "script")) {
  target <- match.arg(target)
  current_selection(target) == ""
}

#' @export
#' @rdname selection-condition-helpers
selection_is_comment_line <- function(target = c("default", "lines", "script")) {
  target <- match.arg(target)
  grepl("^ *#[^\n]*?$",  current_selection(target))
}

#' @export
#' @rdname selection-condition-helpers
selection_is_comment_block <- function(target = c("default", "lines", "script")) {
  target <- match.arg(target)
  grepl("^( *#[^\n]*\n)* *#[^\n]*?$",  current_selection(target))
}

# #' @export
# #' @rdname selection-condition-helpers
# n_lines <- function(txt) {
#   length(current_line_numbers())
# }

#' @export
#' @rdname selection-condition-helpers
selection_is_n_lines <- function(n, target = c("default", "lines", "script")) {
  target <- match.arg(target)
  length(current_line_numbers()) == n
}

#' @export
#' @rdname selection-condition-helpers
selection_is_single_line <- function(target = c("default", "lines", "script")) {
  target <- match.arg(target)
  selection_is_n_lines(1, target)
}

#' @export
#' @rdname selection-condition-helpers
selection_is_parsable <- function(
  multi_ok = TRUE, single_ok = TRUE, symbol_ok = TRUE, empty_ok = FALSE,
  target = c("default", "lines", "script")) {
  target <- match.arg(target)
  if(!empty_ok && selection_is_empty()) return(FALSE)
  res <- (
    single_ok &&  !fails(current_call(target))
  ) || (
    multi_ok && current_selection(target) != "" & !fails(current_expr(target)) && fails(current_call(target))
  )
  if(!symbol_ok) {
    res <- res && !selection_is_symbol()
  }
  res

}

#' @export
#' @rdname selection-condition-helpers
selection_is_evaluable <- function(simple_only = FALSE, target = c("default", "lines", "script")) {
  target <- match.arg(target)
  if(simple_only) {
    selection_is_simple_call() && !fails(current_value(target))
  } else {
    !fails(current_value(target))
  }
}

#' @export
#' @rdname selection-condition-helpers
selection_is_litteral <- function(type = NA) {
  if (!selection_is_parsable(multi_ok = FALSE)) return(FALSE)
  if(is.symbol(current_call())) return(FALSE)
  if(is.na(type)) TRUE else is(current_call(), type)
}

#' @export
#' @rdname selection-condition-helpers
selection_is_reserved_word <-  function() {
  current_selection() %in% c(
    "if", "else", "repeat", "while", "function" ,"for" ,"in", "next", "break",
    "TRUE", "FALSE",  "NULL", "Inf", "NaN", "NA", "NA_integer_" ,"NA_real_", "NA_complex_", "NA_character_")
}

#' @export
#' @rdname selection-condition-helpers
selection_is_symbol <- function(
  litteral_ok = FALSE, reserved_ok = FALSE) {
  if(!litteral_ok && selection_is_litteral()) return(FALSE)
  if(!reserved_ok && selection_is_reserved_word()) return(FALSE)
  selection_is_parsable(multi_ok = FALSE) && is.symbol(current_call())
}

#' @export
#' @rdname selection-condition-helpers
selection_is_call <- function(
  symbol_ok = FALSE, litteral_ok = FALSE, reserved_ok = FALSE,
  target = c("default", "lines", "script")
  ) {
  target <- match.arg(target)
  if(!symbol_ok && selection_is_symbol()) return(FALSE)
  if(!litteral_ok && selection_is_litteral()) return(FALSE)
  if(!reserved_ok && selection_is_reserved_word()) return(FALSE)
  selection_is_parsable(multi_ok = FALSE)
}

# no need to import this one
selection_is_simple_call <- function() {
  selection_is_symbol() || (
    selection_is_call() &&
      deparse1(current_call()[[1]]) %in% c("::", ":::", "[", "[[", "$")
  )
}

#' @export
#' @rdname selection-condition-helpers
selection_contains_string <- function(
  pattern, n_min = 1L, n_max= Inf, target = c("default", "lines", "script"), ...) {
  target <- match.arg(target)
  sum_ <- sum(regexpr(pattern, current_selection(), ...))
  sum_ >= n_min && sum_ <= n_max
}

#' @export
#' @rdname selection-condition-helpers
selection_inherits <- function(class) {
  selection_is_evaluable() && inherits(current_value(), class)
}

#' @export
#' @rdname selection-condition-helpers
selection_is_function <- function() {
  selection_inherits("function")
}

#' @export
#' @rdname selection-condition-helpers
selection_is_data_frame <- function() {
  selection_inherits("data.frame")
}

#' @export
#' @rdname selection-condition-helpers
selection_is_syntactic_package_name <- function() {
  # This should contain only (ASCII) letters, numbers and dot, have at least two
  # characters and start with a letter and not end in a dot
  selection_is_symbol() &&
    grepl("^[[:alpha:]][[:alnum:].]*[^.]$", current_selection())
}

#' @export
#' @rdname selection-condition-helpers
selection_is_installed_package <- function() {
  pkgs <- unlist(sapply(
    .libPaths(), list.dirs, full.names = FALSE, recursive = FALSE),
    use.names = FALSE)
  selection_is_symbol() && current_selection() %in% pkgs
}

#' @export
#' @rdname selection-condition-helpers
selection_is_cran_package <- function() { # (with_github_link = FALSE) ?
  selection_is_symbol() &&
    RCurl::url.exists(
      paste0("https://cran.r-project.org/package=", current_selection()))
}

#' @export
#' @rdname selection-condition-helpers
selection_is_in_rmd_chunk <- function() {
  if (!current_file_is_rmd()) return(FALSE)
  rmd_rows <-
    which(as.logical(cumsum(startsWith(current_file_code(), "```")) %% 2))
  all(current_line_numbers() %in% rmd_rows)
}

#' @export
#' @rdname selection-condition-helpers
selection_is_in_rmd_text <- function() {
  if (!current_file_is_rmd()) return(FALSE)
  txt_rows <-
    which(!cumsum(startsWith(current_file_code(), "```")) %% 2)
  all(current_line_numbers() %in% txt_rows)
}

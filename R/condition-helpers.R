#' Condition Helpers
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
#'   `[[` or `$` are considered. I should be used on condition calls to make sure
#'   we never evaluate a call that would have side effects.
#' @param class class used to check inheritance
#' @export
#' @name condition-helpers
selection_is_empty <- function(txt = current_selection()) {
  txt == ""
}

#' @export
#' @rdname condition-helpers
selection_is_comment_line <- function() {
  grepl("^ *#[^\n]*?$",  current_selection())
}

#' @export
#' @rdname condition-helpers
selection_is_comment_block <- function() {
  grepl("^( *#[^\n]*\n)* *#[^\n]*?$",  current_selection())
}

# #' @export
# #' @rdname condition-helpers
# n_lines <- function(txt) {
#   length(current_line_numbers())
# }

#' @export
#' @rdname condition-helpers
selection_is_n_lines <- function(n) {
  length(current_line_numbers()) == n
}

#' @export
#' @rdname condition-helpers
selection_is_single_line <- function() {
  selection_is_n_lines(1)
}

#' @export
#' @rdname condition-helpers
selection_is_parsable <- function(multi_ok = TRUE, single_ok = TRUE, symbol_ok = TRUE) {
  res <- (
    single_ok &&  !fails(current_call())
  ) || (
    multi_ok && current_selection() != "" & !fails(current_expr()) && fails(current_call())
  )
  if(!symbol_ok) {
    res <- res && !selection_is_symbol()
  }
  res

}

#' @export
#' @rdname condition-helpers
selection_is_evaluable <- function(simple_only = FALSE) {
  if(simple_only) {
    selection_is_simple_call() && !fails(current_value())
  } else {
    !fails(current_value())
  }
}

#' @export
#' @rdname condition-helpers
selection_is_litteral_number <- function() {
  selection_is_parsable(multi_ok = FALSE) &&
    is.numeric(current_call())
}

#' @export
#' @rdname condition-helpers
selection_is_litteral_string <-  function() { # implement (quotes_outside_ok = TRUE)
  selection_is_parsable(multi_ok = FALSE) &&
    is.character(current_call())
}

#' @export
#' @rdname condition-helpers
selection_is_litteral <- function() {
  selection_is_litteral_string() || selection_is_litteral_number()
}

#' @export
#' @rdname condition-helpers
selection_is_reserved_word <-  function() {
  current_selection() %in% c(
    "if", "else", "repeat", "while", "function" ,"for" ,"in", "next", "break",
    "TRUE", "FALSE",  "NULL", "Inf", "NaN", "NA", "NA_integer_" ,"NA_real_", "NA_complex_", "NA_character_")
}

#' @export
#' @rdname condition-helpers
selection_is_symbol <- function(litteral_ok = FALSE, reserved_ok = FALSE) {
  if(!litteral_ok && selection_is_litteral()) return(FALSE)
  if(!reserved_ok && selection_is_reserved_word()) return(FALSE)
  selection_is_parsable(multi_ok = FALSE) && is.symbol(current_call())
}

#' @export
#' @rdname condition-helpers
selection_is_call <- function(symbol_ok = FALSE, litteral_ok = FALSE, reserved_ok = FALSE) {
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

# selection_contains_symbols(symbols=)

#' @export
#' @rdname condition-helpers
selection_inherits <- function(class) {
  selection_is_evaluable() && inherits(current_value(), class)
}


#' @export
#' @rdname condition-helpers
selection_is_function <- function() {
  selection_is_evaluable() && is.function(current_value())
}

# selection_is_data_frame()

#' @export
#' @rdname condition-helpers
selection_is_data_frame <- function() {
  selection_is_evaluable() && is.data.frame(current_value())
}

# selection_is_ggplot()

# selection_is_installed_package()
# selection_is_cran_package(with_github_link = FALSE)
# selection_is_in_rmd_chunk()
# selection_is_in_rmd_text()
# project_is_package()
# project_uses_git()
# project_uses_github()
# project_uses_github_actions()
# project_uses_renv()
# project_has_license(license = NULL)
# file_is_r_script()
# file_is_rmd()
# file_is_rprofile()
# file_is_renviron()
# file_has_extention(ext=)





#' @export
#' @rdname condition-helpers
project_is_package <- function(
  # uses_git = NA,
  # uses_github = NA,
  # uses_c = NA,
  # uses_cran_comments = NA
  ) {
  # mainly built around usethis features
  # We can add infinite features here but should keep them relevant to poof tricks
  # as much as possible
  # TODO:
  # uses_license, can be a license name or logical
  # uses_cpp11
  # uses_citation
  # uses_coverage
  # uses_cran_badge
  # uses_github_actions

  package_bool <- file.exists("DESCRIPTION")
  if(!package_bool) return(FALSE)
  # if(!is.na(uses_git)) {
  #   git_bool <- file.exists(".gitignore")
  #   if(uses_git != git_bool) return(FALSE)
  # }
  # if(!is.na(uses_github)) {
  #   github_bool <- any(grepl("^URL: https://github.com/", readLines("DESCRIPTION")))
  #   if(uses_github != github_bool) return(FALSE)
  # }
  # if(!is.na(uses_c)) {
  #   c_bool <- dir.exists("src")
  #   if(uses_c != c_bool) return(FALSE)
  # }
  # if(!is.na(uses_cran_comments)) {
  #   cran_comments_bool <- file.exists("cran-comments.md")
  #   if(uses_cran_comments != cran_comments_bool) return(FALSE)
  # }
  TRUE
}

current_file_is_r_script <- function() {
  toupper(tools::file_ext(current_path())) == "R"
}

current_file_is_rmd <- function() {
  toupper(tools::file_ext(current_path())) == "RMD"
}


# #' @export
# #' @rdname condition-helpers
# add_args <- function(txt, ...) {
#   poof::replace_selection(deparse(as.call(c(as.list(str2lang(txt)), .groups = 'drop'))))
# }
#
# #' @export
# #' @rdname condition-helpers
# eval_sub <- function(txt) {
#   eval.parent(substitute(debugonce(TXT), list(TXT = as.symbol(txt))))
# }







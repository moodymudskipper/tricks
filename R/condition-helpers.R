#' Condition Helpers
#'
#' These helpers are meant to be used on the left hand side of the trick specification,
#' to filter which tricks the addin should display.
#'
#' @param txt A character object
#' @param to function names whose calls are to be detected
#' @export
#' @name condition-helpers
#' @export
is_comment_line <- function(txt) {
  grepl("^ *#[^\n]*?$", txt)
}

#' @export
#' @rdname condition-helpers
is_comment_block <- function(txt) {
  grepl("^( *#[^\n]*\n)* *#[^\n]*?$", txt)
}

#' @export
#' @rdname condition-helpers
is_parsable <- function(txt) {
  !inherits(try(str2lang(txt), silent = TRUE), "try-error")
}

#' @export
#' @rdname condition-helpers
is_call <- function(txt, to = NULL) {
  res <- poof::is_parsable(txt) && is.call(call <- str2lang(txt))
  if(res && !is.null(to)) {
    res <- as.character(call[[1]]) %in% to
  }
  res
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







#' Clipboard Focused Condition Helpers
#' @export
#' @rdname clipboard-condition-helpers
clipboard_contains_text <- function() {
  w <- options(warn = 2)
  on.exit(options(warn = w$warn))
  !fails(clipboard_text())
}

#' @export
#' @rdname clipboard-condition-helpers
clipboard_is_parsable <- function() {
  clipboard_contains_text() && !fails(parse(text=clipboard_text()))
}

#' @export
#' @rdname clipboard-condition-helpers
clipboard_text <- function() {
  clipr::read_clip()
}

#' @export
#'
#' @rdname clipboard-condition-helpers
clip_board_text_matches <- function(pattern, n_min = 1L, n_max = Inf, ...) {
  clipboard_contains_text() && {
    sum_ <- sum(regexpr(pattern, clipboard_text(), ...))
    sum_ >= n_min && sum_ <= n_max
  }
}

#' @export
#' @rdname condition-helpers
clipboard_contains_text <- function() {
  w <- options(warn = 2)
  on.exit(options(warn = w$warn))
  !fails(clipboard_contains_text())
}

#' @export
#' @rdname condition-helpers
clipboard_is_parsable <- function() {
  clipboard_contains_text() && !fails(parse(text=))
}

#' @export
#' @rdname condition-helpers
clipboard_text <- function() {
  clipr::read_clip()
}

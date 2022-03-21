#' Mock context to test tricks
#'
#' `mock_context()` does some magic so calls to `current_selection()` and other
#' functions using context information (i.e. document content, id, path, and selection(s))
#' will fetch preset data. It is useful to design tests. call `unmock`
#'
#' @param contents A character vector containing lines of code
#' @param id A number
#' @param path A string
#' @param selection A nested list where elements are named lists of `range` created
#'   by `rstudioapi::document_range()` and `position` created by `rstudioapi::document_range()`.
#'   By default `contents` is fully selected, which should be enough for most tests.
#'
#' @return Returns `NULL` invisibly, Called for side effects.
#' @export
mock_context <- function(
  contents = "",
  id = 1,
  path = "",
  selection = NULL
) {
  if(is.null(selection)) {
    n <- length(contents)
    selection <- list(list(
      range = rstudioapi::document_range(
        start = rstudioapi::document_position(1, 1),
        end = rstudioapi::document_position(n, nchar(contents[[n]]))
      ),
      text = paste(contents, collapse = "\n")
    ))
  }
  #
  class(selection) <- "document_selection"
  context <- list(id = id, path = path, contents = contents, selection = selection)
  class(context) <- "document_context"
  globals$mock_context <- context
  invisible(NULL)
}


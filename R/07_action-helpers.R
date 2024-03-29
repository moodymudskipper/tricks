#' Action Helpers
#'
#' These helpers are meant to be used on the right hand side of the trick specification,
#' which is evaluated when the trick is selected.
#'
#' * `replace_selection` replaces the selection in the editor with its argument
#' * `call_addin` calls a standard RStudio addin
#'
#' @param package,name package and addin name
#' @param row,col row and column in he editor
#' @param txt A character object, or will be coerced to character using `deparse()`.
#'   Used as a replacement for the selection text.
#'
#' @export
#' @name action-helpers
call_addin <- function(package, name) {
  if(!requireNamespace(package, quietly = TRUE)) {
    stop("'", package, "' is not installed, consider `install.packages('",
         package, "')`")
  }
  addins <- read.dcf(system.file("rstudio/addins.dcf", package = package))
  fun_nm <- with(as.data.frame(addins), Binding[Name == name])
  getFromNamespace(fun_nm, package)()
}

#' @export
#' @rdname action-helpers
replace_selection <- function(txt) {
  if(!is.character(txt)) txt <- deparse(txt)
  # FIXME : make this work in console
  context   <- current_context("source")
  rstudioapi::modifyRange(context$selection[[c(1,1)]], txt, context$id)
}

#' @export
#' @rdname action-helpers
replace_current_lines <- function(txt) {
  if(!is.character(txt)) txt <- deparse(txt)
  txt <- paste(txt, collapse = "\n")
  context <- current_context("source")
  start_row <- context$selection[[1]]$range$start[["row"]]
  start_col <- 1
  end_row <- context$selection[[1]]$range$end[["row"]]
  end_col <- nchar(context$contents[end_row]) + 1
  rstudioapi::modifyRange(
    rstudioapi::document_range(
      rstudioapi::document_position(start_row, start_col),
      rstudioapi::document_position(end_row,     end_col)
      ), txt, context$id)
}

#' @export
#' @rdname action-helpers
insert_at_position <- function(txt, row=Inf, col=Inf) {
  if(!is.character(txt)) txt <- deparse(txt)
  # FIXME : make this work in console
  context       <- current_context("source")
  rstudioapi::insertText(c(row, col), text = txt, id = context$id)
}


#' @export
#' @rdname action-helpers
send_cursor_at_position <- function(row=1, col=1) {
  # FIXME : make this work in console
  context   <- current_context("source")
  rstudioapi::setCursorPosition(
    rstudioapi::document_position(row,col),
    id = context$id)
}

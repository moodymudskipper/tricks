#' File Focused Condition Helpers
#' @export
#' @rdname file-condition-helpers
document_is_r_script <- function() {
  toupper(tools::file_ext(current_path())) == "R"
}

#' @export
#' @rdname file-condition-helpers
document_is_rmd <- function() {
  toupper(tools::file_ext(current_path())) == "RMD"
}

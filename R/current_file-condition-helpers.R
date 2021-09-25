#' File Focused Condition Helpers
#' @export
#' @rdname file-condition-helpers
current_file_is_r_script <- function() {
  toupper(tools::file_ext(current_path())) == "R"
}

#' @export
#' @rdname file-condition-helpers
current_file_is_rmd <- function() {
  toupper(tools::file_ext(current_path())) == "RMD"
}

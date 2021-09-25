#' @export
#' @rdname condition-helpers
current_file_is_r_script <- function() {
  toupper(tools::file_ext(current_path())) == "R"
}

#' @export
#' @rdname condition-helpers
current_file_is_rmd <- function() {
  toupper(tools::file_ext(current_path())) == "RMD"
}

#' project helpers
#' @export
#' @rdname project-condition-helpers
project_is_package <- function() {
  file.exists("NAMESPACE")
}


#' @export
#' @rdname project-condition-helpers
project_uses_git <- function() {
  file.exists(".gitignore")
}

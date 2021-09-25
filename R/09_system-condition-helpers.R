#' System Focused Condition Helpers
#'
#' @export
system_has_internet <- function() {
  curl::has_internet()
}


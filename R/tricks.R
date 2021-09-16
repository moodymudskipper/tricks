
#' Add, Remove or Show Tricks
#' @param ... tricks to add (or replace), or names of tricks to remove
#' @export
add_tricks <- function(...) {
  new_tricks <- list(...)
  opts <- getOption("poof.tricks")
  opts[names(new_tricks)] <- new_tricks
  options(poof.tricks = opts)
}

#' @rdname add_tricks
#' @export
rm_tricks <- function(...) {
  nms <- c(...)
  if(!length(nms)) {
    options(poof.tricks = list())
  } else {
    opts <- getOption("poof.tricks")
    opts[nms] <- NULL
    options(poof.tricks = opts)
  }
}

#' @rdname add_tricks
#' @export
show_tricks <- function() {
  getOption("poof.tricks")
}

# TODO: edit_tricks to open RProfile, add a call to add_tricks if relevant and place cursor at right place

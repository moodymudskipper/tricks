
#' Add, Remove or Show Tricks
#'
#' Add
#' @param ... tricks to add (or replace), or names of tricks to remove
#' @param .reset whether to remove existing tricks
#' @export
add_tricks <- function(..., .reset = FALSE) {
  if(.reset) rm_tricks()
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

#' @rdname add_tricks
#' @export
edit_tricks <- function() {
  suppressMessages(usethis::edit_r_profile())
  repeat {
    context <- rstudioapi::getSourceEditorContext()
    if(tolower(basename(context$path)) == ".rprofile") break
  }
  file_code <- context$contents
  matches <-  regexpr("poof::add_tricks\\(", file_code)
  row <- which(matches != -1)
  if(length(row)) {
    col <- matches[row] + attr(matches, "match.length")[row]
    send_cursor_at_position(row, col)
  } else {
    txt <- readLines(system.file("default_tricks/default_tricks.R", package = "poof"))
    txt <- paste0(c("", txt), collapse = "\n")
    insert_at_position(txt)
    message("A default `poof::add_tricks()` call has been pasted in you '.RProfile'.",
            "\nYou might modify it or leaving it as is for now!")
  }

  # make all functions available for the time of the call
  if(!"package:poof" %in% search()) {
    library(poof)
    on.exit(detach("package:poof"))
  }

  choice <- select.list(c("Save and restart R to make your changes available", "Cancel"))

  if(choice %in% c("Cancel", "")) {
    rstudioapi::documentClose(context$id, save = FALSE)
  } else {
    rstudioapi::documentClose(context$id, save = TRUE)
    current_key <- fetch_current_hotkey()
    msg <- if(is.null(current_key)) {
      paste(
        sep = "\n",
        "You're almost set! To define a RStudio hotkey go to :",
        "Tools => Modify Keyboard Shortcuts..."
      )
    } else {
      paste0("You're set! Press `", current_key, "` to trigger the addin\n")
    }
    rstudioapi::restartSession(message(msg))
  }
}

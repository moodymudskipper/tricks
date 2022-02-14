
#' Add, Remove or Show Tricks
#'
#' Add
#' @param ... tricks to add (or replace), or names of tricks to remove
#' @param .reset whether to remove existing tricks
#' @export
add_tricks <- function(..., .reset = FALSE) {
  if(.reset) rm_tricks()
  new_tricks_lazy <- eval(substitute(alist(...)))
  # ignore empty args
  new_tricks_lazy <- Filter(function(x) !identical(x, quote(expr=)), new_tricks_lazy)
  pf <- parent.frame()
  new_tricks <- lapply(new_tricks_lazy, eval, pf)
  check_tricks(new_tricks)
  # flatten "tricks_trick" objects
  new_tricks <- unlist(new_tricks)
  tricks <- getOption("tricks.tricks")
  tricks[names(new_tricks)] <- new_tricks
  options(tricks.tricks = tricks)
}

check_tricks <- function(tricks) {
  validate_trick <- function(x, nm) {
    inherits(x, "tricks_trick") || (
      nm != "" && inherits(x, "formula") && length(x) == 3
    )
  }
  validated <- mapply(validate_trick, tricks, allNames(tricks))
  if(!all(validated)) {
    stop(
      "Some tricks were not defined properly.\n",
      paste(deparse(tricks[!validated]), collapse = "\n"),
      call. = FALSE
    )
  }
}

#' @rdname add_tricks
#' @export
rm_tricks <- function(...) {
  nms <- c(...)
  if(!length(nms)) {
    options(tricks.tricks = list())
  } else {
    tricks <- getOption("tricks.tricks")
    tricks[nms] <- NULL
    options(tricks.tricks = tricks)
  }
}

#' @rdname add_tricks
#' @export
show_tricks <- function() {
  getOption("tricks.tricks")
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
  matches <-  regexpr("tricks::add_tricks\\(", file_code)
  row <- which(matches != -1)
  if(length(row)) {
    col <- matches[row] + attr(matches, "match.length")[row]
    send_cursor_at_position(row, col)
  } else {
    txt <- readLines(system.file("default_tricks/default_tricks.R", package = "tricks"))
    txt <- paste0(c("", txt), collapse = "\n")
    insert_at_position(txt)
    message("A default `tricks::add_tricks()` call has been pasted in you '.RProfile'.",
            "\nYou might modify it or leaving it as is for now!")
  }

  # make all functions available for the time of the call
  # if(!"package:tricks" %in% search()) {
  #   message("attaching {tricks} package")
  #   library(tricks)
  #   on.exit(detach("package:tricks"))
  # }
  rstudioapi::sendToConsole("library(tricks)")

  message("Restart your session when you're done for changes to take effect")

  # choice <- select.list(c("Save and restart R to make your changes available", "Cancel"))
  #
  # if(choice %in% c("Cancel", "")) {
  #   rstudioapi::documentClose(context$id, save = FALSE)
  # } else {
  #   rstudioapi::documentClose(context$id, save = TRUE)
  #   current_key <- fetch_current_hotkey()
  #   msg <- if(is.null(current_key)) {
  #     paste(
  #       sep = "\n",
  #       "You're almost set! To define a RStudio hotkey go to :",
  #       "Tools => Modify Keyboard Shortcuts..."
  #     )
  #   } else {
  #     paste0("You're set! Press `", current_key, "` to trigger the addin\n")
  #   }
  #   rstudioapi::restartSession(message(msg))
  # }
}

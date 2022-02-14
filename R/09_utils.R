
#' Test if a Call Fails
#'
#' This is mainly used internally and should be rarely needed but can be useful
#' for some custom trick conditions. Use this with caution! The tested call is run
#' so you don't want it to have potential side effects or be slow!
#'
#' The potential failure will be totally silent (no printed output, no error, no message, no warning).
#' Other side effects should not be an issues if the function is used properly.
#'
#' @param call A call
#' @export
fails <- function(call) {
  pf <- parent.frame()
  capture.output(
    res <- suppressMessages(suppressWarnings(
    eval(substitute(try(call, silent = TRUE),
      environment()
    ), pf)
  ))
  )
  inherits(res, "try-error")
}

fetch_current_hotkey <- function() {
  if(Sys.info()['sysname'] == "Windows") {
    path <- "~/../AppData/Roaming/RStudio/keybindings/addins.json"
  } else {
    path <- "~/.config/rstudio/keybindings/addins.json"
  }
  jsonlite::fromJSON(path)[["tricks::addin"]]
}

fake_selection <- function(new_selection) {
  forget_all()
  current_selection()
  encl <- environment(current_selection)
  env <- environment(encl$`_cache`$get)
  env$value_[[1]][[1]] <- new_selection
  current_selection()
  invisible()
}

called_through_snippets <- function() {
  identical(.rs.rpc.transform_snippet, sys.calls()[[c(1,1)]])
}

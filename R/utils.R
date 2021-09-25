
#' Test if a Call Fails
#'
#' This is mainly used internally and should be rarely needed but can be useful
#' for some custom trick conditions. Use this with caution! The tested call is run
#' so you don't want it to have potential side effects or be slow!
#'
#' @param call A call
#' @export
fails <- function(call) {
  eval.parent(substitute(
    inherits(try(call, silent = TRUE), "try-error")
  ))
}

fetch_current_hotkey <- function() {
  if(Sys.info()['sysname'] == "Windows") {
    path <- "~/../AppData/Roaming/RStudio/keybindings/addins.json"
  } else {
    path <- "~/.config/rstudio/keybindings/addins.json"
  }
  jsonlite::fromJSON(path)[["poof::addin"]]
}

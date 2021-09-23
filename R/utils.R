fails <- function(expr) {
  eval.parent(substitute(
    inherits(try(expr, silent = TRUE), "try-error")
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

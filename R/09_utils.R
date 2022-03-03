
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


# given some incomplete code as a string, return quoted status at end of string
quote_status <- function(txt) {
  # commented characters incl # but not incl \n will be marked as  "commented"\
  # other quotes are ", ', `, and %
  # % cannot be escaped inside of %%

  # remove escaped quotes
  txt <- gsub("\\\\\"", "", txt)
  txt <- gsub("\\\\\'", "", txt)
  txt <- gsub("\\\\\`", "", txt)

  split_txt <- strsplit(txt, "")[[1]]

  i <- 1
  n <- length(split_txt)
  while (i <= n) {
    if (split_txt[[i]] == "#") {
      state <- "commented"
      if(i == n) return(state)
      i <- i + 1
      while(split_txt[[i]] != '\n') {
        if(i == n) return(state)
        i <- i + 1
      }
    } else if (split_txt[[i]] == '"') {
      state <- "double_quoted"
      if(i == n) return(state)
      i <- i + 1
      while(split_txt[[i]] != '"') {
        if(i == n) return(state)
        i <- i + 1
      }
    } else if (split_txt[[i]] == "'") {
      state <- "single_quoted"
      if(i == n) return(state)
      i <- i + 1
      while(split_txt[[i]] != "'") {
        if(i == n) return(state)
        i <- i + 1
      }
    } else if (split_txt[[i]] == "`") {
      state <- "back_quoted"
      if(i == n) return(state)
      i <- i + 1
      while(split_txt[[i]] != "`") {
        state <- "back_quoted"
        if(i == n) return(state)
        i <- i + 1
      }
    } else if (split_txt[[i]] == "%") {
      state <- "infix_quoted"
      if(i == n) return(state)
      i <- i + 1
      while(split_txt[[i]] != "%") {
        state <- "infix_quoted"
        if(i == n) return(state)
        i <- i + 1
      }
    }

    state <- "unquoted"
    if(i == n) return(state)
    i <- i + 1
  }
}

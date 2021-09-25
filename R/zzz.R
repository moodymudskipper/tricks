# memoise functions to avoid redundant computation of the current selection
# We automate the proces for convenience, it includes building programatically
# a `forget_all()` function
local({
  # identify functions to memoise
  env <- parent.frame(2)
  funs     <- ls(envir = env)
  funs     <- grep("^selection_|^current_|^project_|^clipboard_|^system_", funs, value = TRUE)

  # memoise them
  txt <- paste0(funs, " <- memoise::memoise(", funs, ")")
  eval(parse(text = txt), env)

  # build the forget_all() function
  txt <- paste0("memoise::forget(", funs, ")")
  forget_all_body <- as.call(c(quote(`{`), as.list(parse(text = txt))))
  forget_all <- as.function(list(forget_all_body), envir = env)
  assign("forget_all", forget_all, env)
})

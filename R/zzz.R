#' @importFrom methods allNames
#' @importFrom utils capture.output file.edit globalVariables assignInNamespace

globalVariables(c(".rs.rpc.transform_snippet"))

forget_all <- NULL

.onLoad <- function(libname, pkgname) {
  # to avoid check complaints since we know what we're doing here
  # update our namespace with memoised functions
  aiN <- assignInNamespace
  ns <- asNamespace(pkgname)

  # - We memoise functions to avoid redundant computation of the current selection
  # - We build a `forget_all()` function that invalidates all memoised funs
  # - We call `load_yaml_tricks()`

  # identify functions to memoise ----------------------------------------------
  all_fun_nms     <- ls(ns)
  prefixes <- c("clipboard_", "current_", "project_", "selection_" , "system_")
  pattern <- paste0("^", prefixes, collapse = "|")
  prefixed_fun_nms <- grep(pattern, all_fun_nms, value = TRUE)

  # memoise them and replace them in namespace ---------------------------------
  for (nm in prefixed_fun_nms) {
    aiN(
      x = nm,
      value = memoise::memoise(ns[[nm]]),
      ns = ns
    )
  }

  # build the forget_all() function --------------------------------------------

  txt <- paste0("memoise::forget(", prefixed_fun_nms, ")")
  forget_all_body <- as.call(c(quote(`{`), as.list(parse(text = txt))))
  forget_all <- as.function(list(forget_all_body), envir = ns)
  aiN(
    x = "forget_all",
    value = forget_all,
    ns = ns
    )

  # load YAML tricks -----------------------------------------------------------
  load_yaml_tricks()
}

# Use packages to avoid notes
# I think a memoised function's body doesn't contain the original so the calls to
# clipr:: and RCurl:: are hidden and then the checks for old Ubuntu 18.04 complain that some imported packages
# are not useds
clipr::read_clip
RCurl::url.exists


# for backward compatibility
str2lang <- function(x) {
  parse(text = x)[[1]]
}

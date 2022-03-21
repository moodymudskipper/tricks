
#' Add, Remove or Show Tricks
#'
#' Add
#' @param ... tricks to add (or replace), or names of tricks to remove
#' @param .reset whether to remove existing tricks
#' @inheritParams edit_yaml_tricks
#' @export
load_tricks <- function(..., .reset = FALSE) {
  if(.reset) unload_tricks()
  new_tricks <- rlang::dots_list(
    ...,
    .ignore_empty = "all",
    .homonyms = "error"
    )
  new_tricks <- fmls_to_tricks(new_tricks)
  loaded_tricks <- globals$tricks
  list2env(new_tricks, globals$tricks)
  invisible(NULL)
}

#' @export
#' @rdname load_tricks
unload_tricks <- function(...) {
  rm(list = c(...), envir = globals$tricks)
  invisible(NULL)
}

fmls_to_tricks <- function(tricks) {
  fml_to_trick <- function(x, nm) {
    if(inherits(x, "tricks_object")) return(x)
    if (nm == "" | !rlang::is_formula(x, lhs = TRUE)) rlang::abort(
      "`add_tricks()` accepts only trick objects or argument of form `<label> = <condition> ~ <action>`"
    )
    new_trick(nm, rlang::f_lhs(x), rlang::f_rhs(x))
  }
  Map(fml_to_trick, tricks, allNames(tricks))
}

#' @rdname load_tricks
#' @export
uninstall_tricks <- function(..., project_level = FALSE) {
  path <- if (project_level) ".r-tricks.yaml" else "~/.r-tricks.yaml"
  tricks <- yaml_to_trick_list(path)
  nms <- c(...)
  not_found <- setdiff(nms, names(tricks))
  if (length(not_found)) {
    msg <- sprintf(
      "Not found in '%s': %s",
      path,
      toString(paste0("`", not_found, "`"))
    )
    rlang::abort(msg)
  }
  tricks[nms] <- NULL
  trick_list_to_yaml(tricks, path, append = FALSE)
}

#' @rdname load_tricks
#' @export
loaded_tricks <- function() {
  as.list(globals$tricks)
}

#' @rdname load_tricks
#' @export
edit_tricks <- function(project_level = FALSE) {
  message("Attaching {tricks} package, restart your session when you're done for changes to take effect")
  library(tricks)
  path <- if (project_level) ".r-tricks.yaml" else "~/.r-tricks.yaml"
  file.edit(path)
}

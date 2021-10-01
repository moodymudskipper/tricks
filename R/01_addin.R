# to avoid RStudio's margin warning
forget_all <- NULL

#' addin
#' @importFrom utils getFromNamespace select.list
#' @export
addin <- function() {
  if(called_through_snippets()) {
    # snippets hide the output, we free it here
    sink()
    # and sink again just to avoid being warned that there's nothing to unsink
    file <- textConnection("rval", "w", local = TRUE)
    on.exit(sink(file, type = "output", split = FALSE))
  }
  # make all functions available for the time of the call
  if(!"package:poof" %in% search()) {
    library(poof)
    on.exit(detach("package:poof"), add = TRUE)
  }
  # reset all memoised functions
  forget_all()
  # running so it can be memoised
  env <- current_env()
  current_selection()

  opts <- getOption("poof.tricks")
  if(is.null(opts)) {
    message("No actions were defined for this addin")
    return(invisible(NULL))
  }

  # test all conditions to filter eligible tricks
  eval_cond <- function(nm) {
    x <- opts[[nm]]
    lhs_call <- x[[2]]
    res <- try(eval(lhs_call, env), silent = TRUE)
    if(inherits(res, "try-error")) {
      warning(
        "The condition couldn't be evaluated for the trick \"", nm, "\"",
        immediate. = TRUE,
        call. = FALSE)
      return(FALSE)
    }
    res
  }

  conds <- sapply(names(opts), eval_cond)
  if(!any(conds)) {
    message("No tricks to show for this selection")
    return(invisible(NULL))
  }

  rstudioapi::sendToConsole("", FALSE)
  names(opts)[conds] <- sapply(
    names(opts[conds]),
    glue::glue, .envir =env)

  opt_nm <- select.list(names(opts[conds]))
  if(opt_nm == "") {
    rstudioapi::sendToConsole("")
    return(NULL)
  }
  # extract action call
  rhs_call <- opts[[opt_nm]][[3]]
  rhs_call <- do.call(bquote, list(rhs_call))

  eval(rhs_call, env)
  rstudioapi::sendToConsole("")
}

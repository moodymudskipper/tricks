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
  if(!"package:tricks" %in% search()) {
    library(tricks)
    on.exit(detach("package:tricks"), add = TRUE)
  }
  # reset all memoised functions
  forget_all()
  # run so it can be memoised
  current_selection()
  env <- new.env(parent = current_env())
  # this is wrong, we need one env per trick, we should build a list of envs
  # with replicate(new.env(parent = current_env()) and use mapply below
  # we don't use global env because we might call the addin while debugging
   #current_env()


  tricks <- getOption("tricks.tricks")
  if(is.null(tricks)) {
    message("No actions were defined for this addin")
    return(invisible(NULL))
  }

  # test all conditions to filter eligible tricks
  eval_cond <- function(nm) {
    x <- tricks[[nm]]
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

  conds <- sapply(names(tricks), eval_cond)
  if(!any(conds)) {
    message("No tricks to show for this selection")
    return(invisible(NULL))
  }

  rstudioapi::sendToConsole("", FALSE)
  names(tricks)[conds] <- sapply(
    names(tricks[conds]),
    glue::glue, .envir =env)

  trick_label <- select.list(names(tricks[conds]))
  if(trick_label == "") {
    rstudioapi::sendToConsole("")
    return(NULL)
  }
  # extract action call
  rhs_call <- tricks[[trick_label]][[3]]
  rhs_call <- do.call(bquote, list(rhs_call))

  eval(rhs_call, env)
  rstudioapi::sendToConsole("")
}

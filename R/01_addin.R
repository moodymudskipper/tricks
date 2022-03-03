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

  # cache selection and environment
  current_selection()
  current_env()

  tricks <- as.list(globals$tricks)
  if(!length(tricks)) {
    message("No tricks to display")
    return(invisible(NULL))
  }

  # compute condition and label
  tricks_processed <- lapply(tricks, function(x) {
    # provide an env to every trick
    x$.env <- new.env(parent = current_env())
    # eval cond
    x$condition <- try(eval(x$condition, x$.env), silent = TRUE)
    if(inherits(x$condition, "try-error")) {
      warning(
        "The condition couldn't be evaluated for the trick \"", x$label, "\"",
        immediate. = TRUE,
        call. = FALSE)
      x$condition <- FALSE
    } else {
      x$label <- glue::glue(x$label, .envir = x$.env)
    }
    x
  })

  tricks_applicable <- Filter(function(x) x$condition, tricks_processed)

  if(!length(tricks_applicable)) {
    message("No tricks to display")
    return(invisible(NULL))
  }

  rstudioapi::sendToConsole("", FALSE)

  trick_label <- select.list(names(tricks_applicable))
  if(trick_label == "") {
    rstudioapi::sendToConsole("")
    return(NULL)
  }
  # extract action call
  trick <- tricks_applicable[[trick_label]]
  action <- do.call(bquote, list(trick$action))

  eval(action, trick$.env)
  rstudioapi::sendToConsole("")
}



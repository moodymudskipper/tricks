#' addin
#' @importFrom utils getFromNamespace select.list
#' @export
addin <- function() {
  env <- parent.frame()
  context       <- rstudioapi::getSourceEditorContext()
  selection_txt <- rstudioapi::primary_selection(context)[["text"]]
  selection_lng <- try(str2lang(selection_txt), silent = TRUE)
  selection_txt_is_parsable <- !inherits(selection_lng,"try-error")
  selection_txt_is_simple_call <-
    selection_txt_is_parsable &&
    (!is.call(selection_lng) || deparse1(selection_lng[[1]]) %in% c(
      "::", ":::", "[", "[[", "$"))
  if(selection_txt_is_simple_call) {
    selection_val <- try(eval(selection_lng, env))
    selection_txt_is_evaluable <-  !inherits(selection_val, "try-error")
  } else {
    selection_txt_is_evaluable <- FALSE
    selection_val <- NULL
  }

  selection_val <- if(selection_txt_is_simple_call) eval(selection_lng, env)
  opts <- getOption("poof.tricks")
  # test all conditions to filter eligible tricks
  eval_cond <- function(x) {
    if(!selection_txt_is_parsable &&
       any(c(".val", ".lng", ".sub") %in% all.vars(x))
    ) {
      return(FALSE)
    }

    if(!selection_txt_is_evaluable && ".val" %in% all.vars(x)) {
      return(FALSE)
    }

    call <- x[[2]]
    call <- do.call(substitute, list(call, list(
      .txt = selection_txt,
      .lng = substitute(quote(CALL), list(CALL = selection_lng)),
      .sub = selection_lng,
      .val = selection_val)))
    eval(call, env)
  }
  conds <- sapply(opts, eval_cond)
  # send cursor to console
  rstudioapi::sendToConsole("", FALSE)
  names(opts)[conds] <- sapply(
    names(opts[conds]),
    glue::glue,
    .envir = eval(bquote(list(
      .txt = .(selection_txt),
      .lng = quote(.(substitute(quote(CALL), list(CALL = selection_lng)))),
      .sub = .(substitute(quote(CALL), list(CALL = selection_lng))),
      .val = .(selection_val)
    ))))

  opt_nm <- select.list(names(opts[conds]))
  if(opt_nm == "") {
    rstudioapi::sendToConsole("")
    return(NULL)
  }
  # extract action call
  call <- opts[[opt_nm]][[3]]
  # if .val couldn't be evaluated in the first part, eval now
  if(".val" %in% all.vars(call) && !selection_txt_is_evaluable) {
    selection_val <- eval(selection_lng, env)
  }
  # substitute the value
  call <- do.call(substitute, list(call, list(
    .txt = selection_txt,
    .lng = substitute(quote(CALL), list(CALL = selection_lng)),
    .sub = selection_lng,
    .val = selection_val)))
  eval(call, env)
  rstudioapi::sendToConsole("")
}

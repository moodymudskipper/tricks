# maybe we don't need the .txt etc, and we can replace the .sub by the bquote notation

#' addin
#' @importFrom utils getFromNamespace select.list
#' @export
addin <- function() {
  # make all functions available for the time of the call
  if(!"package:poof" %in% search()) {
    library(poof)
    on.exit(detach("package:poof"))
  }
  # reset all memoised functions
  forget_all()
  # running so it can be memoised
  env <- current_env()
  current_selection()

  opts <- getOption("poof.tricks")

  # test all conditions to filter eligible tricks
  eval_cond <- function(nm) {
    x <- opts[[nm]]
    #print(x)
    #browser()
    lhs_call <- x[[2]]
    # vars <- all.vars(x)
    # if(!selection_is_parsable()) {
    #   if (any(c(".val", ".lng", ".sub") %in% vars)) return(FALSE)
    #   lhs_call <- do.call(substitute, list(lhs_call, list(
    #     .txt = current_selection())))
    # } else if(!selection_is_evaluable(simple_only = TRUE)) {
    #   if (".val" %in% vars) return(FALSE)
    #   lhs_call <- do.call(substitute, list(lhs_call, list(
    #     .txt = current_selection(),
    #     .lng = call("quote", current_expr()),
    #     .sub = current_expr())))
    # } else {
    #   lhs_call <- do.call(substitute, list(lhs_call, list(
    #     .txt = current_selection(),
    #     .lng = call("quote", current_expr()),
    #     .sub = current_expr(),
    #     .val = current_value())))
    # }
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

  #browser()
  conds <- sapply(names(opts), eval_cond)
  if(!any(conds)) {
    message("No tricks to show for this selection")
    return(invisible(NULL))
  }
  #browser()
  # send cursor to console
  rstudioapi::sendToConsole("", FALSE)
  names(opts)[conds] <- sapply(
    names(opts[conds]),
    glue::glue, .envir =env)
  # ,
  #   .envir = list(.txt = current_selection())    #   eval(bquote(list(
    #   .txt = .(selection_txt),
    #   .lng = quote(.(substitute(CALL, list(CALL = selection_lng)))),
    #   .sub = .(substitute(quote(CALL), list(CALL = selection_lng))),
    #   .val = .(selection_val)
    # )))
    # )

  opt_nm <- select.list(names(opts[conds]))
  if(opt_nm == "") {
    rstudioapi::sendToConsole("")
    return(NULL)
  }
  # extract action call
  rhs_call <- opts[[opt_nm]][[3]]

  # vars <- all.vars(rhs_call)
  # substitute_list <- list(.txt = current_selection())
  #
  # if(".val" %in% vars) substitute_list[[".val"]] <- current_value()
  # if(".lng" %in% vars) substitute_list[[".lng"]] <- call("quote", current_expr())
  # if(".val" %in% vars) substitute_list[[".sub"]] <- current_expr()
  #
  # rhs_call <- do.call(substitute, list(rhs_call, substitute_list))
  rhs_call <- do.call(bquote, list(rhs_call))

  eval(rhs_call, env)
  rstudioapi::sendToConsole("")
}

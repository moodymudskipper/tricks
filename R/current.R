
#' @export
current_selection <- function() {
  context       <- rstudioapi::getSourceEditorContext()
  selection_txt <- rstudioapi::primary_selection(context)[["text"]]
  selection_txt
}

# this one will work only because called first in addin() and memoised!
current_env <- function() {
  parent.frame()
}

# we probably need a single general function between curren_call and current_expr

current_call <- function() {
  str2lang(current_selection())
}

current_expr <- function() {
  expr <- parse(text = current_selection())
  if(length(expr) == 1) return(expr[[1]])
  as.call(c(quote(`{`), as.list(expr)))
}

current_value <- function() {
  eval(current_expr(), current_env())
}

#' @export
current_line_numbers <- function() {
  context       <- rstudioapi::getSourceEditorContext()
  start_row <- context$selection[[1]]$range$start[["row"]]
  end_row <- context$selection[[1]]$range$end[["row"]]
  start_row:end_row
}

#' @export
current_lines <- function() {
  current_file_code()[current_line_numbers()]
}

#' @export
current_path <- function(full = TRUE) {
  current_code_block()
  context       <- rstudioapi::getSourceEditorContext()
  path <- context$path
  if(!full) path <- basename(path)
  path
}

#' @export
current_file_code <- function() {
  context       <- rstudioapi::getSourceEditorContext()
  context$contents
}

#' @export
current_code_block <- function() {
  code <- current_file_code()
  ind <- current_line_numbers()
  errors <- sapply(seq_along(code), function(i) {
    parsed <- try(parse(text = code[1:i]), silent = TRUE)
    inherits(parsed, "try-error")
  })
  all_block_ids     <- rev(cumsum(rev(!errors)))
  current_block_ids <- unique(all_block_ids[ind])
  code[all_block_ids %in% current_block_ids]
}

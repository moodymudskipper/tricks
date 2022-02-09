
# not sure if we really need curren_lines or document_code in the end,
# now he only difference is we don't collapse with "\n"

#' Current selection
#'
#' Utilities to get data about the selection
#'
#' @param target If `target` is `"lines"` the selection is extended to lines,
#'   if it is `"script"` it is extended to the full script
#' @param full boolean. Whether to return full path or base name
#' @export
current_selection <- function(target = c("default", "lines", "script")) {
  target <- match.arg(target)
  if(target == "lines") return(paste(current_lines(), collapse="\n"))
  if(target == "script") return(paste(document_code(), collapse="\n"))
  context       <- rstudioapi::getSourceEditorContext()
  selection_txt <- rstudioapi::primary_selection(context)[["text"]]
  selection_txt
}

# this one will work only because called first in addin() and memoised!
current_env <- function() {
  parent.frame(2)
}

# we probably need a single general function between current_call and current_expr

#' @export
#' @rdname current_selection
current_call <- function(target = c("default", "lines", "script")) {
  target <- match.arg(target)
  str2lang(current_selection(target = target))
}

#' @export
#' @rdname current_selection
current_expr <- function(target = c("default", "lines", "script")) {
  target <- match.arg(target)
  expr <- parse(text = current_selection(target))
  if(length(expr) == 1) return(expr[[1]])
  as.call(c(quote(`{`), as.list(expr)))
}

#' @export
#' @rdname current_selection
current_value <- function(target = c("default", "lines", "script")) {
  target <- match.arg(target)
  eval(current_expr(target), current_env())
}

#' @export
#' @rdname current_selection
current_line_numbers <- function(target = c("default", "lines", "script")) {
  target <- match.arg(target)
  context       <- rstudioapi::getSourceEditorContext()
  if(target == "script") return(seq_along(context$contents))
  start_row <- context$selection[[1]]$range$start[["row"]]
  end_row <- context$selection[[1]]$range$end[["row"]]
  start_row:end_row
}

#' @export
#' @rdname current_selection
current_lines <- function(target = c("default", "lines", "script")) {
  target <- match.arg(target)
  if(target == "script") return(document_code())
  document_code()[current_line_numbers()]
}

#' @export
#' @rdname current_selection
current_indentation <- function(target = c("default", "lines", "script")) {
  line1 <- current_lines(target)
  chrs <- strsplit(line1, "")[[1]]
  sum(cumprod(chrs == " "))
}

#' @export
#' @rdname current_selection
current_path <- function(full = TRUE) {
  current_code_block()
  context       <- rstudioapi::getSourceEditorContext()
  path <- context$path
  if(!full) path <- basename(path)
  path
}

#' @export
#' @rdname current_selection
document_code <- function() {
  context <- rstudioapi::getSourceEditorContext()
  context$contents
}

#' @export
#' @rdname current_selection
current_code_block <- function() {
  code <- document_code()
  ind <- current_line_numbers()
  errors <- sapply(seq_along(code), function(i) {
    parsed <- try(parse(text = code[1:i]), silent = TRUE)
    inherits(parsed, "try-error")
  })
  all_block_ids     <- rev(cumsum(rev(!errors)))
  current_block_ids <- unique(all_block_ids[ind])
  code[all_block_ids %in% current_block_ids]
}

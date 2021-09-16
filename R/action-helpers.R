#' Action Helpers
#'
#' These helpers are meant to be used on the right hand side of the trick specification,
#' which is evaluated when the trick is selected.
#'
#' * `replace_selection` replaces the selection in the editor with its argument
#' * `call_addin` calls a standard RStudio addin
#'
#' @param package A string. The addin's package
#' @param name A string. The addin's name
#' @param txt A character object, or will be coerced to character using `deparse()`.
#'   Used as a replacement for the selection text.
#'
#' @export
#' @name action-helpers
call_addin <- function(package, name) {
  addins <- read.dcf(system.file("rstudio/addins.dcf", package = package))
  fun_nm <- with(as.data.frame(addins), Binding[Name == name])
  getFromNamespace(fun_nm, package)()
}

#' @export
#' @rdname action-helpers
replace_selection <- function(txt) {
  if(!is.character(txt)) txt <- deparse(txt)
  context       <- rstudioapi::getSourceEditorContext()
  rstudioapi::modifyRange(context$selection[[c(1,1)]], txt, context$id)
}

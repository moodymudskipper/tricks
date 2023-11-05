#' Create a new trick
#'
#' Create a trick object
#'
#' @param label A string
#' @param condition A call or a one sided formula, using condition helpers and context informers, whose
#'   right hand side expressions returns a boolean when evaluated by \pkg{tricks}
#' @param action A call or a one sided formula, using action helpers and context informers, whose
#'   right hand side triggers the desired actions when evaluated by \pkg{tricks}
#' @param description The description of the trick, for documentation purposes
#' @param ... other properties to print with the object and in YAML files
#'
#' @return A list of class "trick_object"
#' @export
#'
#' @examples
#' new_trick(
#' "Edit user '.Rprofile'",
#' ~ selection_is_empty(),
#'~ usethis::edit_r_profile()
#' )
new_trick <- function(label, condition, action, description = label, ...) {

  if (rlang::is_formula(condition)) {
    condition <- condition[[2]]
  }

  if (rlang::is_formula(action)) {
    action <- action[[2]]
  }

  if (!rlang::is_string(label)) {
    rlang::abort("`label` should be a string")
  }

  if (!rlang::is_string(description)) {
    rlang::abort("`description` should be a string")
  }

  if (!is.call(condition)) {
    rlang::abort("`condition` should be a call or a formula")
  }

  if (!is.call(action)) {
    rlang::abort("`action` should be a call or a formula")
  }

  trick <- list(
    label = label,
    description = description,
    condition = condition,
    action = action,
    ...
  )
  structure(trick, class = "trick_object")
}

#' @export
print.trick_object <- function(x, ...) {
  writeLines(c(
    "# A trick for the {tricks} package",
    trick_to_yaml_string(x)
  ))
  invisible(x)
}



#' Read YAML file into a list of trick objects and back
#'
#' You should rarely need to manipulate trick objects. A use case is creating a
#' library of tricks from an existing
#'
#' @param tricks list of tricks to write to `file`
#' @param file Path to a YAML file defining tricks
#' @param append Whether to append to the `file`, if `FALSE` the file will be overwrtitten.
#'
#' @return Returns the first argument invisibly, called for side effects.
#' @export
#'
#' @examples
#' yaml_path <- system.file("tricks.yaml", package = "tricks")
#' tricks <- yaml_to_trick_list(yaml_path)
#' labels <- c("Edit user '.Rprofile'","Edit project '.Rprofile'")
#' trick_subset <- tricks[labels]
#' new_trick_library_path <- tempfile(fileext = ".yaml")
#' trick_list_to_yaml(trick_subset, new_trick_library_path)
yaml_to_trick_list <- function(file) {
  yaml_tricks <- yaml::read_yaml(file)
  #FIXME: validate list
  yaml_to_trick <- function(x, nm) {
    condition <- call("~", str2lang(x$condition))
    action <- call("~", str2lang(x$action))
    new_trick(nm, condition, action)
  }
  tricks <- Map(yaml_to_trick, yaml_tricks, names(yaml_tricks))
  names(tricks) <- names(yaml_tricks)
  tricks
}

#' @rdname yaml_to_trick_list
#' @export
trick_list_to_yaml <- function(tricks, file, append = TRUE) {
  # yaml::as_yaml puts no extra space and doesnt display all code correctly
  trick_to_yaml_string <- function(trick) {
    condition <- deparse(trick[[c(1, 2)]], width.cutoff = 100)
    if (length(condition) > 1) {
      condition <- paste(c("|", condition), collapse = "\n    ")
    }
    action <- deparse(trick[[c(1, 3)]], width.cutoff = 100)
    if (length(action) > 1) {
      action <- paste(c("|", action), collapse = "\n    ")
    }
    paste(
      sep = "\n",
      paste0(names(trick), ":"),
      paste0("  description: ", names(trick)),
      paste0("  condition: ", condition),
      paste0("  action: ", action)
    )
  }
  yaml_tricks <- paste(sapply(tricks, trick_to_yaml_string), collapse = "\n\n")
  if (append && file.exists(file)) yaml_tricks <- paste0(
    paste(readLines(file), collapse = "\n"), "\n\n", yaml_tricks)
  writeLines(yaml_tricks, file)
}


#' Load tricks from YAML file
#'
#' @param file Path to a YAML file, if `NULL` (the default) tricks are loaded
#'   from the user level and project level `.r-tricks.yaml` files if they exist
#' @param labels labels of tricks to load, by default all tricks are loaded
#' @param reset Whether to unload previously loaded tricks
#'
#' @return Returns the first argument invisibly, called for side effects.
#' @export
#'
#' @examples
#' yaml_path <- system.file("tricks.yaml", package = "tricks")
#' load_yaml_tricks(yaml_path)
load_yaml_tricks <- function(file = NULL, labels = NULL, reset = FALSE) {
  if (is.null(file)) {
    if (file.exists(".r-tricks.yaml")) {
      load_yaml_tricks(".r-tricks.yaml")
    }
    if (file.exists("~/.r-tricks.yaml")) {
      load_yaml_tricks("~/.r-tricks.yaml")
    }

    return(invisible(NULL))
  }

  tricks <- yaml_to_trick_list(file)
  if (!is.null(labels)) {
    #FIXME validate labels
    tricks <- tricks[labels]
  }
  do.call(add_tricks, c(tricks, .reset = reset))
  invisible(file)
}

#' Use YAML tricks
#'
#' Sets up YAML tricks for user or project
#'
#' * Edits user level or project level ".RProfile" to add a call to `load_yaml_tricks()`
#' * Optionally runs installation of tricks from {tricks} package (default for user level YAML)
#' * Adds the project level `.r-tricks.yaml` file to `.Rbuildignore`
#'
#' @param project_level Whether to setup tricks at the project level
#'
#' @return Returns the input invisibly, called for side effects.
#' @export
use_yaml_tricks <- function(project_level = FALSE) {
  if (project_level) {
    project_yaml <- ".r-tricks.yaml"
    project_rprofile <- ".Rprofile"
    usethis::use_build_ignore(project_yaml)
    if (!file.exists(project_rprofile)) {
      message("Created project level '.Rprofile' loading tricks from project level '.r-tricks.yaml'")
      lines <- "load_yaml_tricks(project_level = TRUE)"
    } else {
      lines <- readLines(project_rprofile)
      load_line <- "tricks::load_yaml_tricks(project_level = TRUE)"
      if(! load_line %in% lines) {
        message("Updated project level '.Rprofile' to load tricks from project level '.r-tricks.yaml'")
        lines <- paste(c(lines, load_line), collapse = "\n")
      } else {
        message("The project level '.Rprofile' is already set to load tricks from the project level '.r-tricks.yaml'")
      }
    }
    writeLines(lines, project_rprofile)
  } else {
    user_file <- "~/.r-tricks.yaml"
    user_rprofile <- "~/.Rprofile"
    if (!file.exists(user_rprofile)) {
      message("Created user level '.Rprofile' loading tricks from user level '.r-tricks.yaml'")
      lines <- "load_yaml_tricks()"
    } else {
      lines <- readLines(user_rprofile)
      load_line <- "tricks::load_yaml_tricks()"
      if(! load_line %in% lines) {
        message("Updated user level '.Rprofile' to load tricks from user level '.r-tricks.yaml'")
        lines <- paste(c(lines, load_line), collapse = "\n")
      } else {
        message("The user level '.Rprofile' is already set to load tricks from the user level '.r-tricks.yaml'")
      }
    }
    writeLines(lines, user_rprofile)
  }

  install_tricks("tricks", project_level, all = TRUE)

  message(
    "Call `edit_yaml_tricks()` to remove/edit installed tricks or add your own\n",
    "Make sure that you've defined a shortcut for this package's addin 'Trigger `tricks::addin()`'\n",
    "Call `install_tricks()` to install new tricks from packages or YAML files\n"
    )

  invisible(project_level)
}


#' Install tricks from a package or path
#'
#' @param source The name of a package containing tricks., or a path to a YAML
#' file defining tricks, By default proposes tricks from the {tricks} package
#' @param project_level whether to install tricks at the project level (in "./.r-tricks.yaml"),
#'   by default they're installed at the user level (in "~/.r-tricks.yaml")
#' @param all whether to install all tricks without prompting
#'
#' @return Returns the first argument invisibly, called for side effects.
#' @export
install_tricks <- function(source = "tricks", project_level = FALSE, all = FALSE) {
  if(project_level) {
    file_to <- ".r-tricks.yaml"
  } else {
    file_to <- "~/.r-tricks.yaml"
  }
  if (file.exists(file_to)) {
    old_tricks <- yaml_to_trick_list(file_to)
  } else {
    old_tricks <- NULL
  }
  old_labels <- names(old_tricks)

  source_is_yaml <- grepl("\\.yaml$", source)
  if(source_is_yaml) {
    file_from <- source
  } else {
    file_from <- system.file("tricks.yaml", package = source)
  }
  tricks <- yaml_to_trick_list(file_from)
  tricks <- tricks[!names(tricks) %in% old_labels]
  if (all) {
    new_tricks <- tricks
  } else {
    nums <- select.list(
      names(tricks),
      multiple = TRUE,
      title = "select tricks to add to your '.r-tricks.yaml' file")
    new_tricks <- tricks[nums]
  }

  new_labels <- names(new_tricks)

  # common_labels <- intersect(new_labels, old_labels)
  # if(length(common_labels)) {
  #   stop("You already have tricks installed with labels: \n", paste("*", common_labels, collapse = "\n"))
  # }
  trick_list_to_yaml(new_tricks, file_to)
  if (length(new_labels))
    message("The following tricks were installed:\n", paste("*", new_labels, collapse = "\n"))
  invisible(source)
}


#' Edit YAML tricks
#'
#' @param project_level whether to open the project level file ("./.r-tricks.yaml"),
#'   by default opens the user level file ("~/.r-tricks.yaml")
#'
#' @return Returns the input invisibly, called for side effects.
#' @export
edit_yaml_tricks <- function(project_level = FALSE) {
  if (project_level) {
    file <- ".r-tricks.yaml"
  } else {
    file <- "~/.r-tricks.yaml"
  }
  file.edit(file)
  rstudioapi::sendToConsole("library(tricks)")
  message("Restart your session when you're done for changes to take effect")
  invisible(project_level)
}

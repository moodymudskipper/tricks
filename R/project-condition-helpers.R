#' project helpers
#' @export
#' @rdname project-condition-helpers
project_is_package <- function(
  # uses_git = NA,
  # uses_github = NA,
  # uses_c = NA,
  # uses_cran_comments = NA
) {
  # mainly built around usethis features
  # We can add infinite features here but should keep them relevant to poof tricks
  # as much as possible
  # TODO:
  # uses_license, can be a license name or logical
  # uses_cpp11
  # uses_citation
  # uses_coverage
  # uses_cran_badge
  # uses_github_actions

  package_bool <- file.exists("DESCRIPTION")
  if(!package_bool) return(FALSE)
  # if(!is.na(uses_git)) {
  #   git_bool <- file.exists(".gitignore")
  #   if(uses_git != git_bool) return(FALSE)
  # }
  # if(!is.na(uses_github)) {
  #   github_bool <- any(grepl("^URL: https://github.com/", readLines("DESCRIPTION")))
  #   if(uses_github != github_bool) return(FALSE)
  # }
  # if(!is.na(uses_c)) {
  #   c_bool <- dir.exists("src")
  #   if(uses_c != c_bool) return(FALSE)
  # }
  # if(!is.na(uses_cran_comments)) {
  #   cran_comments_bool <- file.exists("cran-comments.md")
  #   if(uses_cran_comments != cran_comments_bool) return(FALSE)
  # }
  TRUE
}


#' @export
#' @rdname project-condition-helpers
project_uses_git <- function() {
  file.exists(".gitignore")
}


# file_is_r_script()
# file_is_rmd()
# file_is_rprofile()
# file_is_renviron()
# file_has_extention(ext=)


# project_uses_github()
# project_uses_github_actions()
# project_uses_renv()
# project_has_license(license = NULL)

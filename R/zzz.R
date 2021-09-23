# memoise functions to avoid redundant computation of the current selection
# and selection helpers

current_call                 <- memoise::memoise(current_call)
current_code_block           <- memoise::memoise(current_code_block)
current_env                  <- memoise::memoise(current_env)
current_expr                 <- memoise::memoise(current_expr)
current_file_code            <- memoise::memoise(current_file_code)
current_file_is_r_script     <- memoise::memoise(current_file_is_r_script)
current_file_is_rmd          <- memoise::memoise(current_file_is_rmd)
current_line_numbers         <- memoise::memoise(current_line_numbers)
current_lines                <- memoise::memoise(current_lines)
current_path                 <- memoise::memoise(current_path)
current_selection            <- memoise::memoise(current_selection)
current_value                <- memoise::memoise(current_value)
selection_is_call            <- memoise::memoise(selection_is_call)
selection_is_comment_block   <- memoise::memoise(selection_is_comment_block)
selection_is_comment_line    <- memoise::memoise (selection_is_comment_line)
selection_is_empty           <- memoise::memoise(selection_is_empty)
selection_is_evaluable       <- memoise::memoise(selection_is_evaluable)
selection_is_litteral        <- memoise::memoise(selection_is_litteral)
selection_is_litteral_number <- memoise::memoise(selection_is_litteral_number)
selection_is_litteral_string <- memoise::memoise(selection_is_litteral_string)
selection_is_n_lines         <- memoise::memoise(selection_is_n_lines)
selection_is_parsable        <- memoise::memoise(selection_is_parsable)
selection_is_reserved_word   <- memoise::memoise(selection_is_reserved_word)
selection_is_simple_call     <- memoise::memoise(selection_is_simple_call)
selection_is_single_line     <- memoise::memoise(selection_is_single_line)
selection_is_symbol          <- memoise::memoise(selection_is_symbol)

# relevant_funs <- Filter(
#   function(x) startsWith(x, "current_") || startsWith(x, "selection_is"),
#   ls(asNamespace("poof")))
# cat(paste0(relevant_funs, " <- memoise::memoise(", relevant_funs, ")", collapse = "\n"))
# cat(paste0("memoise::forget(", relevant_funs, ")", collapse = "\n"))


forget_all <- function() {
  memoise::forget(current_call)
  memoise::forget(current_code_block)
  memoise::forget(current_env)
  memoise::forget(current_expr)
  memoise::forget(current_file_code)
  memoise::forget(current_file_is_r_script)
  memoise::forget(current_file_is_rmd)
  memoise::forget(current_line_numbers)
  memoise::forget(current_lines)
  memoise::forget(current_path)
  memoise::forget(current_selection)
  memoise::forget(current_value)
  memoise::forget(selection_is_call)
  memoise::forget(selection_is_comment_block)
  memoise::forget(selection_is_comment_line)
  memoise::forget(selection_is_empty)
  memoise::forget(selection_is_evaluable)
  memoise::forget(selection_is_litteral)
  memoise::forget(selection_is_litteral_number)
  memoise::forget(selection_is_litteral_string)
  memoise::forget(selection_is_n_lines)
  memoise::forget(selection_is_parsable)
  memoise::forget(selection_is_reserved_word)
  memoise::forget(selection_is_simple_call)
  memoise::forget(selection_is_single_line)
  memoise::forget(selection_is_symbol)
}

.onLoad <- function(libname, pkgname){
  add_tricks(
    .reset = TRUE,
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # No selection - general
    "Edit '.Rprofile'" =
      selection_is_empty() ~
      usethis::edit_r_profile(),
    "Edit '.Renviron'" =
      selection_is_empty() ~
      usethis::edit_r_environ(),
    "Edit RStudio snippets" =
      selection_is_empty() ~
      usethis::edit_rstudio_snippets(),
    "Paste clipboard content as a tribble with 'datapasta'" =
      selection_is_empty() ~
      replace_selection(datapasta::tribble_paste()),
    "Lint current file with 'lintr'" =
      selection_is_empty() ~
      call_addin("lintr", "Lint current file"),
    "Embed a Lockfile with 'renv'" =
      selection_is_empty() ~
      call_addin("renv", "Embed a Lockfile"),
    "Browse RStudio addins" =
      selection_is_empty() ~
      call_addin("addinslist", "Browse RStudio addins"),
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # No selection - in a package project
    "Use git" =
      selection_is_empty() &&
      project_is_package(uses_git = FALSE) ~
      usethis::use_git(),
    "Use github" =
      selection_is_empty() &&
      project_is_package(uses_github = FALSE) ~
      usethis::use_github(),
    "Use cran-comments"  =
      selection_is_empty() &&
      project_is_package(uses_cran_comments = FALSE) ~
      usethis::use_cran_comments(),
    "Calculate package test coverage with `covr::report()`"  =
      selection_is_empty() &&
      project_is_package(uses_cran_comments = FALSE) ~
      covr::report(),
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # No selection - in a Rmd file
    "Add GIF with 'giphyr'" = # you might want the github version on windows
      selection_is_empty() && current_file_is_rmd() ~
      call_addin("giphyr", "Add GIFs"),
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Selecting (potential) package names
    "visit CRAN page of '{.txt}' package" =
      selection_is_symbol() ~
      usethis::browse_cran(.txt),
    "visit github page of '{.txt}' package" =
      selection_is_symbol() ~
      usethis::browse_github(.txt),
    "Install '{.txt}' package from CRAN" =
      selection_is_symbol() ~
      remotes::install_cran(.txt, upgrade = "ask"),
    "Install '{.txt}' package  from github" =
      selection_is_symbol() ~
      remotes::install_github(paste(strsplit(usethis:::github_url(.txt), "/")[[1]][4:5], collapse = "/"), upgrade = "ask"),
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # selecting a function name
    "debugonce({.txt})" =
      selection_is_function() ~
      debugonce(.sub),
    "View flow diagram of '{.txt}' function" =
      selection_is_function() ~
      print(flow::flow_view(.sub)),
    "Rig function with `boomer::rig_in_namespace()`" =
      selection_is_function() ~
      boomer::rig_in_namespace(.sub),
    "Lookup '{.txt}' on 'rdocumentation.org" =
      selection_is_symbol() ~
      browseURL(paste0("https://www.rdocumentation.org/search?q=", .txt)),
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # selecting a data frame
    "Make ggplot2 plots using 'esquisse'" =
      selection_is_data_frame() ~
      call_addin("esquisse", "'ggplot2' builder"),
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # selecting a ggplot object
    "Edit ggplot object with 'ggedit'" =
      selection_inherits("ggplot") ~
      ggedit::ggedit(.val),
    # changes not pasted right atm
    "Edit ggplot object with 'ggThemeAssist'" =
      selection_inherits("ggplot") ~
      local({
        res <- ggThemeAssist::ggThemeAssistGadget(.sub)
        replace_selection(res)
      }),
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # selecting calls
    # View flow diagram of '{deparse1(.lng[[1]])}' call" =
    # to fix we need to re-implement other substitutions
    "View flow diagram of call" =
      selection_is_call() ~
      print(flow::flow_run(.sub)),
    "Explode call wih `boomer::boom()`" =
      selection_is_call()  ~
      call_addin("boomer", "Explode a call wih `boom()`"),
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # selecting multiline code
    "Reprex selection" =
      selection_is_parsable(symbol_ok = FALSE) ~
      call_addin("reprex", "Reprex selection"),
    "carbonate" =
      selection_is_parsable(symbol_ok = FALSE) ~
      call_addin("carbonate", "carbonate"),
    "Value to clipboard" =
      selection_is_parsable() ~
      clipr::write_clip(.sub),
    "Style selection wih 'styler'" =
      selection_is_parsable() ~
      call_addin("styler", "Style selection"),
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # initiate the first iteration of for loop
    "{gsub('^for\\\\((.*?) in (.*?)\\\\)$', '\\\\1', .txt)} <- ({gsub('^for\\\\((.*?) in (.*?)\\\\)$', '\\\\2', .txt)})[[1]]" =
      grepl("^for\\((.*?) in (.*?)\\)$", .txt) ~
      eval(call("<-",
                str2lang(gsub('^for\\((.*?) in (.*?)\\)$', '\\1', .txt)),
                call('[[', str2lang(gsub('^for\\((.*?) in (.*?)\\)$', '\\2', .txt)), 1))),
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Convert `summarize(...) var1, var2` to `group_by(var1, var2) %>% summarize(..., .groups = "drop")`
    "add group_by() and .groups = \"drop\" argument" =
      grepl("^( *summari[zs]e\\(.*?)\\) ([1-9a-zA-Z._, ]+)$", .txt) ~
      replace_selection({
        sub(
          "^( *summari[zs]e\\(.*?)\\) ([1-9a-zA-Z._, ]+)$", "group_by(\\2) %>% \\1, .groups = 'drop')", .txt)
      }),
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    "allign `<-` assignments" =
      selection_is_parsable() && sum(grepl("<-", strsplit(.txt, "\n")[[1]])) > 1 ~
      local({
        code <- current_lines()
        pos <- regexpr("<-", code)
        pad <- max(pos) - pos
        code <- mapply(function(x, n) sub("<-", paste0(strrep(" ", n), "<-"), x), code, pad)
        replace_current_lines(code)
      }),
    "allign comments" =
      selection_is_parsable() && sum(grepl("#", strsplit(.txt, "\n")[[1]])) > 1 ~
      local({
        code <- current_lines()
        pos <- regexpr("#", code)
        pad <- max(pos) - pos
        code <- mapply(function(x, n) sub("#", paste0(strrep(" ", n), "#"), x), code, pad)
        replace_current_lines(code)
      })
  )
}

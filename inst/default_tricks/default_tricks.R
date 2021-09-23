poof::add_tricks(
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

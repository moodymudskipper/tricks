poof::add_tricks(
  .reset = TRUE,
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # No selection - general
  "Edit tricks" =
    selection_is_empty() ~
    edit_tricks(),

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
    selection_is_empty() &&
    clipboard_contains_text() &&
    !clipboard_is_parsable() &&
    !fails(datapasta::tribble_construct()) ~
    replace_selection(datapasta::tribble_construct()),

  "Lint current file with 'lintr'" =
    selection_is_empty() ~
    call_addin("lintr", "Lint current file"),

  "Embed a Lockfile with 'renv'" =
    selection_is_empty() ~
    call_addin("renv", "Embed a Lockfile"),

  "Browse RStudio addins" =
    selection_is_empty() ~
    call_addin("addinslist", "Browse RStudio addins"),
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # No selection - in a package project
  "Use git" =
    selection_is_empty() &&
    project_is_package() &&
    !project_uses_git() ~
    usethis::use_git(),

  "Calculate package test coverage with `covr::report()`"  =
    selection_is_empty() &&
    project_is_package() ~
    covr::report(),

  "Add GIF with 'giphyr'" = # you might want the github version on windows
    selection_is_empty() && selection_is_in_rmd_text() ~
    call_addin("giphyr", "Add GIFs"),

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Selecting (potential) package names
  "visit CRAN page of '{current_selection()}' package" =
    selection_is_cran_package() ~
    usethis::browse_cran(current_selection()),

  "visit github page of '{current_selection()}' package" =
    selection_is_cran_package() ~
    usethis::browse_github(current_selection()),

  "Install '{current_selection()}' package from CRAN" =
    selection_is_cran_package() ~
    remotes::install_cran(current_selection(), upgrade = "ask"),

  "Install '{current_selection()}' package  from github" =
    selection_is_cran_package()  ~
    remotes::install_github(paste(strsplit(usethis:::github_url(current_selection()), "/")[[1]][4:5], collapse = "/"), upgrade = "ask"),

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # selecting a function name
  "debugonce({current_selection()})" =
    selection_is_function() ~
    debugonce(.(current_call())),

  "View flow diagram of '{current_selection()}' function" =
    selection_is_function() ~
    print(flow::flow_view(.(current_call()))),

  "Rig function with `boomer::rig_in_namespace()`" =
    selection_is_function() ~
    boomer::rig_in_namespace(.(current_call())),

  "Lookup '{current_selection()}' on 'rdocumentation.org" =
    selection_is_symbol() ~
    browseURL(paste0("https://www.rdocumentation.org/search?q=", current_selection())),
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
      res <- ggThemeAssist::ggThemeAssistGadget(.(current_call()))
      replace_selection(res)
    }),
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # selecting calls
  # View flow diagram of '{deparse1(current_call()[[1]])}' call" =
  # to fix we need to re-implement other substitutions
  "View flow diagram of call" =
    selection_is_call() ~
    print(flow::flow_run(.(current_call()))),

  "Explode call wih `boomer::boom()`" =
    selection_is_call()  ~
    call_addin("boomer", "Explode a call wih `boom()`"),

  "Refactor chunk wih 'refactor' package" =
    selection_is_parsable() ~
    local({
      new_code <- paste(
        "{\n",
        current_selection(),
        "\n\n#~ for {refactor}:\nrm()\n} %refactor_chunk% {\n",
        current_selection(),
        "\n\n#~ for {refactor}:\nrm()\n}")
      new_code <-
        styler::style_text(new_code, base_indention = current_indentation())
      new_code <- paste(new_code, collapse = "\n")
      replace_current_lines(new_code)
    }),
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
    clipr::write_clip(.(current_call())),

  "Style selection wih 'styler'" =
    selection_is_parsable(symbol_ok = FALSE) ~
    call_addin("styler", "Style selection"),
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # initiate the first iteration of for loop
  "{gsub('^for\\\\((.*?) in (.*?)\\\\)$', '\\\\1', current_selection())} <- ({gsub('^for\\\\((.*?) in (.*?)\\\\)$', '\\\\2', current_selection())})[[1]]" =
    grepl("^for\\((.*?) in (.*?)\\)$", current_selection()) ~
    eval(call("<-",
              str2lang(gsub('^for\\((.*?) in (.*?)\\)$', '\\1', current_selection())),
              call('[[', str2lang(gsub('^for\\((.*?) in (.*?)\\)$', '\\2', current_selection())), 1))),
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert `summarize(...) var1, var2` to `group_by(var1, var2) %>% summarize(..., .groups = "drop")`
  "add group_by() and .groups = \"drop\" argument" =
    grepl("^( *summari[zs]e\\(.*?)\\) ([1-9a-zA-Z._, ]+)$", current_selection()) ~
    replace_selection({
      sub(
        "^( *summari[zs]e\\(.*?)\\) ([1-9a-zA-Z._, ]+)$", "group_by(\\2) %>% \\1, .groups = 'drop')", current_selection())
    }),
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  "allign `<-` assignments" =
    selection_contains_string("<-", n_min = 2, target = "lines") ~
    local({
      code <- current_lines()
      pos <- regexpr("<-", code)
      pad <- max(pos) - pos
      code <- mapply(function(x, n) sub("<-", paste0(strrep(" ", n), "<-"), x), code, pad)
      replace_current_lines(code)
    }),

  "allign comments" =
    selection_contains_string("#", n_min = 2, target = "lines") ~
    local({
      code <- current_lines()
      pos <- regexpr("#", code)
      pad <- max(pos) - pos
      code <- mapply(function(x, n) sub("#", paste0(strrep(" ", n), "#"), x), code, pad)
      replace_current_lines(code)
    })
)

poof::add_tricks(
  "debugonce({.txt})" =    # use a glue string to have a dynamic item name
    is.function(.val) ~  # consider only selection that evaluates to a function
    debugonce(.sub)      # run function, by substituting the selection in the action
)

# Now if I select a function the debugonce trick is proposed
ave(1:3, c(1,1,2))



call_addin("giphyr", "Add GIFs")

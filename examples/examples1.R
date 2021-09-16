poof::add_tricks(
  "Load all" =
    .txt == "" ~          # only consider if no selection
    devtools::load_all(),  # call load_all()
  "Turn comments to upper case" =
    poof::is_comment_block(.txt) ~          # only consider if selection is a comment block
    poof::replace_selection(toupper(.txt)), # replace selection with upper case
  "Reprex selection" =
    TRUE ~                                           # always consider
    poof::call_addin("reprex", "Reprex selection")  # call existing addin
)

# I've set the {poof} addin to my favorite hotkey

# If I select nothing the 1st and 3rd tricks are proposed
# let's load_all() !



# If I select this comment, the 2nd and 3rd tricks are proposed
# let's convert to upper case !

# If I select the some code, only the 3rd will be proposed
# let's reprex !
answer <- 42


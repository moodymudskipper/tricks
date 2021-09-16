poof::add_tricks(
  "Reprex selection" =
    is.call(.lng) ~                                 # consider only parsable calls
    poof::call_addin("reprex", "Reprex selection")  # call existing addin
)

# Now if I select nothing only the load_all() trick is proposed
# let's skip it (type 0)



# If I select this comment, only the toupper trick is proposed
# let's skip it (type 0)

# If I select the some valide code, I still have my reprex trick!
# let's reprex !
answer <- 42


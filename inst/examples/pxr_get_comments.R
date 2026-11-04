# Needs internet access.
# The comments of a table at Statistics Sweden, on the table, its variables and values
kommentarer <- pxr_get_comments("scb", "TAB638")
kommentarer[, c("niva", "var_namn", "varde_namn", "obligatorisk")]

# Only the comments that the provider says must be shown with the figures
kommentarer$kommentar[kommentarer$obligatorisk]

# Several tables at once; the column tabellid tells them apart
pxr_get_comments("ssb", c("07459", "03013"))

# PxWeb API v1: the National Institute of Economic Research has comments on values.
# This takes a few calls, since they only come with data
pxr_get_comments("konj", "indikatorer/Indikatorm.px")

# Statistics Denmark has comments on the whole table only
pxr_get_comments("dst", "FU17", lang = "en")

# Needs internet access.
codes <- pxr_get_all_codes("scb", "TAB638")
codes$Kon
codes$ContentsCode

# provider is "scb" by default, so it can be left out by naming tab
pxr_get_all_codes(tab = "TAB638")$Kon

# Several tables: every variable that some table has, with the codes of them all
lengths(pxr_get_all_codes(tab = c("TAB5557", "TAB638")))

# The three latest years
tail(codes$Tid, 3)

# Codes and labels side by side for one variable
labs <- pxr_get_all_labels("scb", "TAB638")
data.frame(code = codes$Civilstand, label = labs$Civilstand)

# A query that takes the first value of every variable
query <- lapply(codes, function(x) x[1])
str(query)
pxr_pxweb2_get_data("TAB638", query = query)

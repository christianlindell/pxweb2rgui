# Needs internet access.
labs <- pxr_get_all_labels("scb", "TAB638")
names(labs)
labs$Kon
head(labs$Region)

# Several tables: every variable that some table has, with the labels of them all
pxr_get_all_labels(tab = c("TAB5557", "TAB638"))$Civilstand

# How many values each variable has
lengths(labs)

# The same table in English
pxr_get_all_labels("scb", "TAB638", lang = "en")$Civilstand

# Statistics Norway
pxr_get_all_labels("ssb", "07459")$Kjonn

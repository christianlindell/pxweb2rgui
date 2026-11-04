# Needs internet access. The metadata of TAB638 is fetched once and then
# reused from the session cache.
# Labels of the regions that end with "l\u00e4n" (the counties)
pxr_get_labels("scb", "TAB638", "Region", ends_with = "l\u00e4n")

# Their codes instead of the labels
pxr_get_labels("scb", "TAB638", "Region", ends_with = "l\u00e4n", to_codes = TRUE)

# provider is "scb" by default, so it can be left out by naming tab and var
pxr_get_labels(tab = "TAB638", var = "Kon")

# Several tables: the labels are combined without duplicates
pxr_get_labels(tab = c("TAB5557", "TAB638"), var = "Civilstand")

# Several filters at once: start with "S", but not a county
pxr_get_labels("scb", "TAB638", "Region", starts_with = "S", not_contains = "l\u00e4n")

# Upper and lower case do not matter, and var can be the variable's label
pxr_get_labels("SCB", "TAB638", "region", contains = "BORG")

# Several strings in one filter: either of them is enough
pxr_get_labels("scb", "TAB638", "Region", ends_with = c("holm", "k\u00f6ping"))

# Short labels: municipalities whose names have three letters
pxr_get_labels("scb", "TAB638", "Region", n_chars = 3)

# The codes of chosen municipalities, ready for a query
kommuner <- pxr_get_labels("scb", "TAB638", "Region",
                           starts_with = c("Falun", "Borl\u00e4nge"), to_codes = TRUE)
pxr_pxweb2_get_data("TAB638", query = list(
  Region = kommuner, Civilstand = NA, Alder = "tot", Kon = NA,
  ContentsCode = "BE0101N1", Tid = "9999"))

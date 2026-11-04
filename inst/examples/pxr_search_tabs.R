# Needs internet access.
# Tables whose names contain "folkmängd" (upper or lower case does not matter)
pxr_search_tabs("scb", search_str = "folkmängd")

# Several texts: tables about either births or deaths
pxr_search_tabs("scb", search_str = c("födda", "döda"))

# Only tables updated since 1 September 2026
pxr_search_tabs("scb", updated_since = "2026-09-01", search_str = "befolkning")

# Statistics Norway, English table names
pxr_search_tabs("ssb", lang = "en", search_str = "population")

# Needs internet access. Listing all tables of a provider takes a few seconds.
# All tables of Statistics Sweden
alla <- pxr_get_all_tabs("scb")
head(alla)
nrow(alla)

# Only tables updated since 1 October 2026
pxr_get_all_tabs("scb", updated_since = "2026-10-01")

# Statistics Norway, table names in English
head(pxr_get_all_tabs("ssb", updated_since = "2026-10-01", lang = "en"))

# A site with PxWeb API v1: the Swedish Board of Agriculture. tabellid is the
# table's path, and is used as it is in the other functions.
head(pxr_get_all_tabs("sjv"))

# Region Västra Götaland: one provider per database
pxr_get_all_tabs("vgr_demografi")

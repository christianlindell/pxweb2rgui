# Needs internet access. The metadata of TAB638 is fetched once and then
# reused from the session cache.
# Municipalities in Dalarna: codes with four characters that start with "20"
pxr_get_codes("scb", "TAB638", "Region", starts_with = "20", n_chars = 4)

# Their names instead of the codes
pxr_get_codes("scb", "TAB638", "Region", starts_with = "20", n_chars = 4,
              to_labels = TRUE)

# All counties: two-character codes, without "00" (the whole country)
pxr_get_codes("scb", "TAB638", "Region", n_chars = 2, not_contains = "00")

# Ages 60-69: two-character codes that start with "6"
pxr_get_codes("scb", "TAB638", "Alder", starts_with = "6", n_chars = 2)

# Years ending in 0 or 5, from 2000 onwards
pxr_get_codes("scb", "TAB638", "Tid", starts_with = "20", ends_with = c("0", "5"))

# Statistics Norway: regions whose codes start with "03" (Oslo)
pxr_get_codes("ssb", "07459", "Region", starts_with = "03")

# PxWeb API v1: years from 2020 onwards in a table of the Swedish Board of
# Agriculture. The table id is the table's path.
pxr_get_codes("sjv", "Animalieproduktion/Mejeriproduktion/JO0604A1.px", "År",
              to_labels = TRUE, starts_with = "2")

# Statistics Denmark: the sexes in the population table FOLK1A
pxr_get_codes("dst", "FOLK1A", "KØN", lang = "en")

# Several tables: the codes are combined without duplicates. Tables that lack
# the variable are passed over.
pxr_get_codes("scb", c("TAB5557", "TAB638"), "Tid")

# Fetch the population of every municipality in Dalarna, latest year
dalarna <- pxr_get_codes("scb", "TAB638", "Region", starts_with = "20", n_chars = 4)
pxr_pxweb2_get_data("TAB638", query = list(
  Region = dalarna, Civilstand = NA, Alder = "tot", Kon = NA,
  ContentsCode = "BE0101N1", Tid = "9999"))

# The examples need internet access. Each call fetches only a few cells.

# Population of Dalarna county (code "20") by sex in the latest year.
# Civilstand (marital status) is eliminated with NA; Tid = "9999" means the
# latest period.
pop <- pxr_pxweb2_get_data(
  "TAB638",
  query = list(Region = "20", Civilstand = NA, Alder = "tot", Kon = "*",
               ContentsCode = "BE0101N1", Tid = "9999")
)
pop

# The same in English, with a column of region codes (region_code) as well
pxr_pxweb2_get_data(
  "TAB638",
  query = list(Region = "20", Civilstand = NA, Alder = "tot", Kon = "*",
               ContentsCode = "BE0101N1", Tid = "9999"),
  lang = "en",
  code_variables = "Region"
)

# Labels work as values too: "Dalarnas län" instead of "20"
pxr_pxweb2_get_data(
  "TAB638",
  query = list(Region = "Dalarnas län", Civilstand = NA, Alder = "tot",
               Kon = NA, ContentsCode = "BE0101N1", Tid = c("2023", "2024"))
)

# Several tables: TAB5557 has 2025 and TAB638 has the years up to 2024, so each
# table is called with its own years and contents, and the rows are combined.
# The column table_id says where a row comes from.
pxr_pxweb2_get_data(
  c("TAB5557", "TAB638"),
  query = list(Region = "20", Civilstand = "OG", Alder = "20", Kon = "*",
               ContentsCode = c("000007ME", "BE0101N1"), Tid = c("2024", "2025"))
)

# Another provider: Statistics Norway, population of Oslo in the latest year
pxr_pxweb2_get_data(
  "07459",
  query = list(Region = "0301", Kjonn = NA, Alder = NA,
               ContentsCode = "Personer1", Tid = "9999"),
  base_url = "ssb"
)

# Statistics Denmark: population of Copenhagen by sex in the latest quarter.
# Variables that are left out with NA are eliminated.
pxr_pxweb2_get_data(
  "FOLK1A",
  query = list(`OMRÅDE` = "101", `KØN` = c("1", "2"), ALDER = NA, CIVILSTAND = NA, Tid = "9999"),
  base_url = "dst",
  lang = "en"
)

# PxWeb API v1: the Swedish Board of Agriculture. The table id is the table's
# path, and "9999" and labels work as in API v2.
pxr_pxweb2_get_data(
  "Animalieproduktion/Mejeriproduktion/JO0604A1.px",
  query = list(Variabel = "Kvantitet invägd mjölk, 1 000 ton", `År` = "9999"),
  base_url = "sjv"
)

# Region Västra Götaland: one provider per database
pxr_pxweb2_get_data(
  "Arbetsmarknad/forvarvsfrekvens.px",
  query = list(tid = "2000", region = "Sverige", `kön` = "män", `ålder` = "20-64"),
  base_url = "vgr_arbetsmarknad"
)

# The address of any site with API v1 works too, with its language and database
pxr_pxweb2_get_data(
  "indikatorer/Indikatorm.px",
  query = list(Indikator = "KIFI", Period = "top(3)"),
  base_url = "https://statistik.konj.se/PxWeb/api/v1/sv/KonjBar/"
)

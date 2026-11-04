# pxweb2rgui

pxweb2r och pxweb2rgui i ett paket. Paketet hämtar data och metadata från
statistikdatabaser med PxWeb API v1 eller v2, som standard SCB:s statistikdatabas.
Andra leverantörer, till exempel SSB och Danmarks Statistik, och alla språk en leverantör erbjuder
kan användas. Det grafiska gränssnittet, som också finns som RStudio-addin,
låter dig välja leverantör, språk, tabell, variabler och värden, och ger
färdig `pxr_pxweb2_get_data()`-kod.

Paketet bygger på pxweb2r 0.0.0.9011 av Peter Möller
([FaluPeppe/pxweb2r](https://github.com/FaluPeppe/pxweb2r)) och pxweb2rgui
0.5.1. Paketet exporterar funktionerna som börjar med `pxr_` och det grafiska
gränssnittet.

## Installation

```r
install.packages(c("bslib", "dplyr", "DT", "htmltools", "httr", "jsonlite",
                   "purrr", "rjstat", "rlang", "shiny", "stringr", "tibble",
                   "tidyr"))
install.packages("C:/Users/chris/OneDrive/R_claude/pxweb2rtgui/pxweb2rgui_0.20.1.tar.gz",
                 repos = NULL, type = "source")
```

Rekommenderade extrapaket: `writexl`, `nanoparquet`, `clipr`, `janitor` och
`rstudioapi`. Starta om RStudio efter installationen så att addin:et syns i
menyn *Addins* som *Statistiktabell till pxweb2r-kod*.

Paketet ersätter det äldre pxweb2rgui 0.5.x. Det kan installeras samtidigt som pxweb2r,
men ladda bara ett av dem åt gången, eftersom flera funktioner har samma namn.

## Användning

```r
library(pxweb2rgui)

# Folkmängden i Dalarnas kommuner det senaste året
kommuner <- pxr_get_codes("scb", "TAB638", "Region", starts_with = "20", n_chars = 4)
pxr_pxweb2_get_data("TAB638", query = list(
  Region = kommuner, Civilstand = NA, Alder = "tot", Kon = NA,
  ContentsCode = "BE0101N1", Tid = "9999"))

# Tabeller om folkmängd som uppdaterats sedan 1 oktober 2026
pxr_search_tabs("scb", updated_since = "2026-10-01", search_str = "folkmängd")

# Det grafiska gränssnittet
pxweb2_gui()
```

Inställningar som `options(pxweb2r.lang = "en")`,
`options(pxweb2r.base_url = "ssb")` och `options(pxweb2rgui.provider = "ssb")`
heter som förut. Gränssnittets tabellista cachas i
`tools::R_user_dir("pxweb2rgui", "cache")`.

## Leverantörer

`provider` är `"scb"` som standard i alla funktioner som har argumentet. Utan
`provider` namnger du övriga argument, till exempel
`pxr_get_all_codes(tab = "TAB638")`. I `pxr_pxweb2_get_data()` anges samma namn
som `base_url`, med SCB som standard. Namnen kan skrivas med stora eller små
bokstäver.

| Namn | Statistikmyndighet | API |
|---|---|---|
| `"scb"` | Statistiska centralbyrån, Sverige | v2 |
| `"ssb"` | Statistisk sentralbyrå, Norge | v2 |
| `"dst"` | Danmarks Statistik, Danmark | eget API |
| `"statfin"` | Statistikcentralen, Finland | v1 |
| `"fohm"` | Folkhälsomyndigheten, Sverige | v1 |
| `"sjv"` | Jordbruksverket, Sverige | v1 |
| `"tillvaxtanalys"` | Tillväxtanalys, Sverige | v1 |
| `"konj"` | Konjunkturinstitutet, Sverige | v1 |
| `"skolverket"` | Skolverket, Sverige | v1 |
| `"vgr_arbetsmarknad"` | Västra Götalandsregionen: Arbetsmarknad och näringsliv | v1 |
| `"vgr_demografi"` | Västra Götalandsregionen: Demografi | v1 |
| `"vgr_folkhalsa"` | Västra Götalandsregionen: Folkhälsoenkäten | v1 |
| `"vgr_gransregional"` | Västra Götalandsregionen: Gränsregional statistik Norge-Sverige | v1 |
| `"vgr_personal"` | Västra Götalandsregionen: Personalstatistik | v1 |
| `"nordicstats"` | Nordic Statistics (Nordregio), Norden | v1 |

Västra Götalandsregionens server har fem databaser, och varje databas är en egen
leverantör. Flera tabeller i samma anrop måste därför ligga i samma databas.

Gränssnittet visar namnet för den valda leverantören under fliken "1. Hitta tabell".

## PxWeb API v1

Alla exporterade funktioner fungerar också mot webbplatser med API v1. Versionen
avgörs av adressen: `/v1/` (eller `/v0/`) i adressen är API v1. Adressen ska ha
med språket och gärna databasen. Tabell-id är tabellens sökväg, som
`pxr_get_all_tabs()` ger i kolumnen `tabellid`.

**En leverantör som har både API v1 och v2 används alltid med v2**, även om du
anger adressen till dess v1, och ett meddelande säger det. Det gäller SCB och
SSB. Ange språket med `lang`, eftersom språket i v1-adressen då inte används. För
övriga webbplatser kontrolleras en gång per session om v2 finns, så en leverantör
som går över till v2 följs automatiskt. Tabellerna har andra id i v2, så en
sökväg från v1, till exempel `"BE/BE0101/BE0101A/BefolkningNy"`, ger ett fel som
förklarar det. Det nya id:t hittar du med `pxr_search_tabs()`.

I API v1 är tabell-id tabellens sökväg. Ett id utan mappar, till exempel `"JO0604A1"`,
med eller utan `.px` och med stora eller små bokstäver, fungerar också: paketet tar
reda på sökvägen i tabellistan innan webbplatsen tillfrågas. Finns namnet i flera mappar,
som `"EDUC11"` hos Nordic Statistics, får du ett fel som listar sökvägarna, eftersom
tabeller med samma namn kan ha olika innehåll. Första gången under en session läser ett
sådant id in tabellistan, vilket kan ta några minuter på en stor webbplats utan söktjänst.

```r
pxr_get_all_tabs("sjv")
pxr_pxweb2_get_data(
  "Animalieproduktion/Mejeriproduktion/JO0604A1.px",
  query = list(Variabel = "Kvantitet invägd mjölk, 1 000 ton", `År` = "9999"),
  base_url = "sjv"
)
pxr_get_all_tabs("https://statistik.konj.se/PxWeb/api/v1/sv/KonjBar/")
```

| Leverantör | Webbplats |
|---|---|
| `"statfin"` | Statistikcentralen, Finland |
| `"fohm"` | Folkhälsomyndigheten |
| `"sjv"` | Jordbruksverket |
| `"tillvaxtanalys"` | Tillväxtanalys |
| `"konj"` | Konjunkturinstitutet |
| `"skolverket"` | Skolverket |
| `"vgr_arbetsmarknad"`, `"vgr_demografi"`, `"vgr_folkhalsa"`, `"vgr_gransregional"`, `"vgr_personal"` | Västra Götalandsregionen |
| `"nordicstats"` | Nordic Statistics, statistik om alla nordiska länder från Nordregio (bara engelska) |

Det här finns inte i API v1, eller fungerar annorlunda:

| Funktion | Skillnad i API v1 |
|---|---|
| `pxr_get_all_tabs()`, `pxr_search_tabs()` | Ingen tabellista finns. Listan tas fram med webbplatsens sökning, och där den saknas läses alla mappar. På en stor webbplats kan det ta några minuter första gången i en session, och ett meddelande säger det |
| `pxr_pxweb2_get_data()` | Inga kodlistor: `include_aggregations` ger en varning och ignoreras, och kolumnen `kommun` är tom för DeSO-områden, också med en varning. `kommun_kod` är komplett, och RegSO-områden får båda |
| `pxr_pxweb2_get_data()` | Enstaka tabeller finns men kan inte levereras som JSON-stat av servern, som är det enda format paketet läser. Felmeddelandet säger det |
| `pxweb2_gui()`, `pxweb2_gui_addin()` | "Välj indelning" finns inte, avslutade tabeller går inte att skilja ut och perioder visas inte i tabellistan. Gränssnittet säger det när webbplatsen väljs |
| `pxr_get_labels()`, `pxr_get_codes()`, `pxr_get_all_labels()`, `pxr_get_all_codes()` | Ingen skillnad |
| `pxr_get_comments()` | Kommentarerna finns inte i metadatan, så funktionen gör en liten datahämtning per variabel och två till. API:et säger sällan vilka kommentarer som är obligatoriska, och då är `obligatorisk` `NA` |

API v1 har heller ingen lista över språk. För en webbplats utanför paketets
leverantörslista erbjuder gränssnittet bara språket i adressen, medan `lang =`
i funktionerna provas mot webbplatsen.

## Danmarks Statistik

Danmarks Statistik har ett eget API som inte är PxWeb, men funktionerna används på
samma sätt med leverantören `"dst"`. Språken är `"da"` (standard) och `"en"`.

```r
pxr_search_tabs("dst", search_str = "population", lang = "en")
pxr_get_codes("dst", "FOLK1A", "KØN", lang = "en")
pxr_pxweb2_get_data(
  "FOLK1A",
  query = list(`OMRÅDE` = "101", `KØN` = c("1", "2"), ALDER = NA, CIVILSTAND = NA, Tid = "9999"),
  base_url = "dst",
  lang = "en"
)
```

API:et har inga kodlistor, så `include_aggregations` ger en varning och ignoreras,
och "Välj indelning" finns inte i gränssnittet. Resultatet har alltid en kolumn med
tabellinnehållet som heter `Indhold`, även med `lang = "en"`. Stora hämtningar delas
upp som vanligt, och avslutade tabeller och perioder fungerar i tabellistan.

## Flera tabeller

`pxr_pxweb2_get_data()` tar en vektor med tabell-id. Varje tabell anropas med de
variabler och värden den har, och raderna slås ihop med `dplyr::bind_rows()`.
Kolumnen `table_id` visar vilken tabell en rad kommer från.

```r
# TAB5557 har 2025 och TAB638 har åren till och med 2024
pxr_pxweb2_get_data(
  c("TAB5557", "TAB638"),
  query = list(Region = "20", Civilstand = "OG", Alder = "20", Kon = "*",
               ContentsCode = c("000007ME", "BE0101N1"), Tid = c("2024", "2025"))
)

# Koderna från båda tabellerna, utan dubbletter
pxr_get_codes("scb", c("TAB5557", "TAB638"), "Tid")
```

* En tabell som saknar en variabel anropas utan den. En variabel som en tabell har
  men `query` inte nämner får alla sina värden, som med en tabell; `NA` utelämnar den.
* Har en tabell inget av de valda värdena i en variabel den kan utelämna, anropas
  den utan variabeln, så att raderna gäller alla värden i variabeln. Väljer du
  "totalt, samtliga civilstånd" i TAB5557 och TAB638 får du totalen från båda, trots
  att TAB638 saknar det värdet. Väljer du i stället ett värde som inte är en total
  gäller samma regel, så titta på meddelandet och på `NA` i kolumnen.
* En tabell anropas inte om den saknar de valda värdena i en variabel den kräver,
  till exempel tid, om en sådan variabel är tom (`NA`), eller om den saknar de valda
  värdena i en region, eftersom resultatet annars skulle gälla hela landet. Ett
  meddelande säger det, och `on_all_values_invalid = "stop"` ger fel först när ingen
  tabell kan anropas.
* Du kan välja värden i listor för variabler som bara vissa tabeller har. Kolumner som
  bara vissa tabeller har blir `NA` på de andra tabellernas rader, som med
  `dplyr::bind_rows()`, och en varning säger vilka.
* `pxr_get_labels()`, `pxr_get_codes()`, `pxr_get_all_labels()` och
  `pxr_get_all_codes()` slår ihop värdena utan dubbletter. Tabeller som saknar
  variabeln hoppas över, och det blir fel först om ingen tabell har den.

I gränssnittet väljer du flera tabeller med Ctrl+klick i trädet, även mellan
sökningar. Flik 2 visar då variabler som alla tabeller har för sig, och variabler
som bara finns i vissa tabeller för sig med en rad om vilka tabeller som har dem.
Variabler med samma kod blir en lista med värdena från alla tabeller.

Knappen "Kommentarer" i flik 1 är gråtonad tills en eller flera tabeller är valda. Ett
klick hämtar samma kommentarer som `pxr_get_comments()` och öppnar fliken "4. Kommentarer",
som är gråtonad så länge den saknar innehåll. Varje tabell får en egen ruta med tabell-id
och tabellnamn, så att det syns vilken tabell en kommentar hör till. Kommentarerna raderas när valet
av tabeller ändras eller töms, när leverantör eller språk byts och när appen avslutas.

Hämtat data i flik 3 raderas när något ändras som påverkar vad som hämtas, till exempel
valda värden. Rutorna för kodkolumner och "R-kompatibla kolumnnamn" hämtar inget på nytt
utan ändrar tabellen direkt, och nedladdningen ger det som visas.

Indelningar med samma namn blir ett val under "Välj indelning", även när id:na
skiljer sig mellan tabellerna, till exempel "Riket", "Län" och "Kommuner" i TAB638
och TAB5557. Namnen måste stämma frånsett stora och små bokstäver. En indelning
som bara vissa tabeller har visas med "(bara TAB638)", och tabeller utan dess värden
hämtas inte.

Samma namn betyder inte alltid samma tal, och samma kod inte alltid samma sak.
Därför gäller i gränssnittet:

* En enda tabell behåller alla sina rader. Namn som står flera gånger får koden efter
  sig, till exempel "100+ år (100+5)" i TAB5557 och "monetärt guld (FL1100)" i TAB1194.
* Samma kod och samma namn i flera tabeller blir en rad.
* Olika koder med samma namn, till exempel "Folkmängd" i TAB638 och TAB5557, blir en
  rad om varje tabell har högst en kod med namnet. Varje tabell hämtas med sin kod.
* Har samma kod olika namn i olika tabeller, som månaderna hos Jordbruksverket, väljs
  värdena på namn, och koden får namnen i `query`. Regioner slås aldrig ihop på namn.

`"9999"` är varje tabells egen senaste period, och gäller bara tabellens tidsvariabel.

## Funktioner

Varje exporterad funktion har en hjälpsida med ett körbart exempel. Exemplen kräver
internet. Vignetterna `vignette("pxweb2r", package = "pxweb2rgui")` och
`vignette("pxweb2rgui", package = "pxweb2rgui")` visar hur funktionerna
och gränssnittet används.

| Funktion | Vad den gör | Obligatoriska argument |
|---|---|---|
| `pxr_get_all_tabs()` | Alla tabeller hos en leverantör, med uppdateringsdatum | – |
| `pxr_search_tabs()` | Tabeller vars namn innehåller en text | `search_str` |
| `pxr_get_labels()` | Etiketter för en variabel, filtrerade på text | `tab`, `var` |
| `pxr_get_codes()` | Koder för en variabel, filtrerade på text | `tab`, `var` |
| `pxr_get_all_labels()` | Alla etiketter i en tabell, per variabel | `tab` |
| `pxr_get_all_codes()` | Alla koder i en tabell, per variabel | `tab` |
| `pxr_get_comments()` | Kommentarer till en tabell, dess variabler och värden | `tab` |
| `pxr_pxweb2_get_data()` | Hämta data | `table` |
| `pxweb2_gui()` | Öppna det grafiska gränssnittet | – |
| `pxweb2_gui_addin()` | RStudio-addin: statistiktabell till kod | – |

Övriga funktioner från pxweb2r, till exempel `pxweb2_get_metadata()` och
`pxweb2_search_tables()`, är interna från version 0.3.0. De nås vid behov med
`pxweb2rgui:::`.

## Versionshistorik

### 0.20.1 (2026-11-04)

#### Rättat

* `pxr_get_comments()` och fliken "4. Kommentarer" med PxWeb API v1: kommentarer på
  tabellen och på variabler som webbplatsen bara lämnar i px-formatet kommer nu med. Hos
  Nordic Statistics saknades till exempel alla nio kommentarer på tabellen WORK30, och hos
  Konjunkturinstitutet en på `indikatorer/Indikatorm.px`. Funktionen gör ett anrop mer per
  tabell. För dessa kommentarer anger `obligatorisk` också vad som måste visas.

### 0.20.0 (2026-11-04)

#### Ändrat

* **Bakåtinkompatibelt:** paketet heter nu `pxweb2rgui`, inte `pxweb2rguisamlat`. Använd
  `library(pxweb2rgui)` i stället för `library(pxweb2rguisamlat)`, och samma sak i
  `pxweb2rguisamlat::`. Koden från gränssnittet börjar med det nya namnet. Paketet ersätter
  det äldre pxweb2rgui 0.5.x, och tabellistan sparas i cachen för `pxweb2rgui`.
* Gränssnittet: märkningen "Obligatorisk" och fetstilen för sådana kommentarer är borta,
  så alla kommentarer i fliken "4. Kommentarer" ser likadana ut. Kolumnen `obligatorisk`
  i `pxr_get_comments()` finns kvar.
* Gränssnittets rubrik är nu "pxweb2rgui".

#### Rättat

* `pxr_get_comments()`: en kommentar som leverantören anger med värdet `false` i
  `noteMandatory` får nu `obligatorisk = FALSE`. Tidigare blev den `TRUE`.

### 0.19.0 (2026-11-04)

#### Ändrat

* Gränssnittet: hämtat data i fliken "3. Kod och data" raderas så fort något ändras som
  påverkar vad som hämtas: valda tabeller, leverantör, språk, valda värden, indelning
  eller "Hämta alltid senaste perioden". En text säger att data behöver hämtas på nytt.
* Rutorna för kodkolumner och för "R-kompatibla kolumnnamn" ändrar tabellen direkt, åt
  båda hållen och utan ny hämtning. Nedladdningen ger det som visas, och koden följer
  rutorna som tidigare.

### 0.18.0 (2026-11-04)

#### Ändrat

* Gränssnittet: kryssrutan "Visa kommentarer för vald tabell" i fliken "2. Välj
  variabler" är ersatt av knappen "Kommentarer" i fliken "1. Hitta tabell". Knappen är
  gråtonad tills en eller flera tabeller är valda. Ett klick hämtar kommentarerna och
  öppnar fliken "4. Kommentarer".
* Fliken "4. Kommentarer" finns alltid, men är gråtonad och går inte att klicka på när
  den saknar innehåll.
* Kommentarerna raderas när valet av tabeller ändras eller töms, när leverantör eller
  språk byts och när appen avslutas. Klicka på knappen igen för att hämta dem för det
  nya valet.

### 0.17.0 (2026-11-04)

#### Nytt

* Gränssnittet: kryssrutan "Visa kommentarer för vald tabell" i fliken "2. Välj
  variabler" ger den nya fliken "4. Kommentarer" med kommentarerna från
  `pxr_get_comments()`. Med flera valda tabeller får varje tabell en egen ruta med
  tabell-id och tabellnamn, och obligatoriska kommentarer är märkta. Länkar i
  kommentarerna går att klicka på.

### 0.16.0 (2026-11-04)

#### Nytt

* `pxr_get_comments()` hämtar kommentarerna (fotnoterna) till en eller flera tabeller,
  en rad per kommentar, med vad kommentaren gäller (tabell, variabel eller värde) och
  om leverantören säger att den måste visas tillsammans med siffrorna. Den fungerar
  mot API v2, PxWeb API v1 och Danmarks Statistik.

#### Rättat

* I PxWeb API v1 tar paketet självt reda på sökvägen till ett entydigt tabell-id utan
  mappar, med eller utan `.px` och med stora eller små bokstäver, innan webbplatsen
  tillfrågas. Webbplatserna behandlar sådana id olika: hos till exempel
  `"vgr_gransregional"` gick det tidigare inte utan `.px`. Kolumnen `tabellid` och koden
  från gränssnittet har fortfarande hela sökvägen.

### 0.15.1 (2026-11-04)

#### Ändrat

* I PxWeb API v1 ger ett tabell-id utan mappar ett fel som listar de fullständiga
  sökvägarna när tabeller i flera mappar har det namnet, i alla funktioner som tar ett
  tabell-id. Hos `"nordicstats"` gäller det till exempel `"EDUC11"`, som finns i två
  mappar med olika åldersgrupper och enheter. **Bakåtinkompatibelt:** tidigare valdes
  den första tabellen utan varning. Ange i stället sökvägen från kolumnen `tabellid` i
  `pxr_get_all_tabs()`. Entydiga id och fullständiga sökvägar fungerar som förut.

### 0.15.0 (2026-11-04)

#### Nytt

* Nordic Statistics (nordicstatistics.org), Nordregios statistikdatabas med statistik
  om alla nordiska länder, finns som leverantören `"nordicstats"`. Den använder PxWeb
  API v1 och finns bara på engelska.

### 0.14.0 (2026-11-03)

#### Ändrat

* `"9999"` med flera tabeller är varje tabells egen senaste period, i tabellens ordning
  och bara i dess tidsvariabel. **Bakåtinkompatibelt:** tidigare hämtades bara den
  senaste perioden bland alla tabeller, och tabeller utan den hoppades över.
* Gränssnittet: en enda tabell behåller alla sina rader, och namn som står flera gånger
  får koden efter sig. **Bakåtinkompatibelt:** "100+ år" i SCB:s TAB5557 är åter flera
  rader, och alla värden valda ger `"*"`.

#### Rättat

* Gränssnittet: värden med samma namn men olika tal försvann ur listan, till exempel
  17 av 43 koder i SCB:s TAB1194. Sedan 0.10.0 gick de inte att välja eller hämta.
* `"9999"` och tidslistan sorterade koder som text, så att fel period kunde väljas där
  koderna är löpnummer, till exempel hos Jordbruksverket.
* Gränssnittet: samma kod med olika namn i olika tabeller blandades ihop. Nu väljs
  sådana värden på namn, och varje tabell hämtas med sin egen kod.
* En tabell kan inte längre få samma tal två gånger genom en hopslagen rad.
* En variabels kod går före en annan variabels namn, så att `Region` i en tabell inte
  ändrar valet för en annan variabel med namnet region i en annan tabell.
* Jokertecken, `from()`, `to()` och `range()` fungerar med flera tabeller.
* `"9999"` stoppar inte längre alla tabeller när en tabell saknar tydlig tidsvariabel,
  och en vanlig kod 9999 i en annan variabel byts inte ut.
* Ett `agg_`-värde hämtar inte alla regioner i en tabell utan den aggregeringen.
* `NA` i en obligatorisk variabel hoppar över tabellen även med
  `on_all_values_invalid = "*"`, och `NULL` behandlas som `NA`.
* `allow_label_values = FALSE` gäller också när flera tabeller filtreras.
* Svar 429 utan rubriken Retry-After väntar och försöker igen i stället för att krascha.
* Gränssnittet: en tabell som inte går att läsa ändrar inte längre valet eller indelningen.

### 0.13.1 (2026-11-03)

#### Ändrat

* Flera tabeller: en tom lista, eller `NA` i `pxr_pxweb2_get_data()`, stoppar bara om
  alla tabeller kräver variabeln. Annars hoppas de tabeller som kräver den över, med
  ett meddelande, och övriga hämtas. I gränssnittet visas "(Obligatoriskt)" och den
  röda ramen bara när listan måste fyllas i.

#### Rättat

* Gränssnittet: antalet celler i fliken "2. Välj variabler" uppdateras när valen ändras.

### 0.13.0 (2026-11-03)

#### Nytt

* Västra Götalandsregionens statistikdatabaser finns som leverantörer, en per databas:
  `"vgr_arbetsmarknad"`, `"vgr_demografi"`, `"vgr_folkhalsa"`, `"vgr_gransregional"`
  och `"vgr_personal"`. De använder PxWeb API v1.

#### Rättat

* PxWeb API v1: en tabell där webbplatsen inte markerat tidsvariabeln behandlas ändå
  rätt om variabeln har ett vanligt namn, som tid, år eller månad. Då fungerar `"9999"`
  för senaste perioden, och gränssnittet ger förval och visar "Hämta alltid senaste
  perioden".
* Gränssnittet skrev varningen "Unknown or uninitialised column: `common`" i konsolen.

### 0.12.1 (2026-11-03)

#### Ändrat

* Gränssnittet med flera tabeller: kryssrutan "Visa gemensamma värden" finns nu för
  alla variabler, även obligatoriska.

### 0.12.0 (2026-11-03)

#### Nytt

* Gränssnittet med flera tabeller: frivilliga variabler som flera tabeller har får
  kryssrutan "Visa gemensamma värden" i flik 2. Den döljer och avmarkerar värden som
  inte finns i alla tabeller som har variabeln.

### 0.11.1 (2026-11-03)

#### Ändrat

* Gränssnittet med flera tabeller: en variabel som är obligatorisk i vissa tabeller
  och kan hoppas över i andra säger det, till exempel "Obligatorisk i TAB5557, kan
  hoppas över i TAB638."

### 0.11.0 (2026-11-03)

#### Ändrat

* `pxr_pxweb2_get_data()` med flera tabeller och gränssnittet: en tabell som saknar de
  valda värdena i en variabel den kan utelämna hämtas nu utan variabeln, i stället
  för att hoppas över. Raderna gäller då alla värden i variabeln, och ett meddelande
  säger det. I gränssnittet visas det som "TAB638: 2 (utan civilstånd, kön)". Tid och
  andra variabler som tabellen kräver, och regioner, gör fortfarande att tabellen
  inte hämtas. **Bakåtinkompatibelt:** samma fråga kan ge fler rader än förut.
* Gränssnittet med flera tabeller: värden som bara vissa tabeller har märks i listan,
  till exempel "totalt, samtliga civilstånd (bara TAB5557)". Meddelandet om en tabell
  som inte hämtas nämner variablerna, och koden får en kommentarsrad om tabeller som
  hämtas utan en variabel eller inte hämtas alls.

### 0.10.1 (2026-11-03)

#### Rättat

* `pxr_pxweb2_get_data()`: med en kodkolumn, till exempel `code_variables = "Alder"`,
  försvann talet ur klartexten, så att "20 år" blev "år". Koden tas nu bara bort för
  regioner och variabler där metadata säger att koden visas med värdet.

### 0.10.0 (2026-11-03)

#### Nytt

* Gränssnittet: "Välj indelning" finns nu också med flera tabeller. Indelningar med
  samma namn blir ett val även när id:na skiljer sig, och en indelning som bara vissa
  tabeller har visas med "(bara ...)".

#### Rättat

* Gränssnittet: värden med samma namn men olika koder visas som en rad, och samma tal
  hämtas inte flera gånger. Det gäller till exempel åldrarna i SCB:s TAB5557 och
  tabellinnehåll med samma namn i flera tabeller. Är alla sådana värden valda listas
  koderna i koden i stället för `"*"`.

### 0.9.0 (2026-11-03)

#### Nytt

* Gränssnittet: Ctrl+klick i trädet väljer flera tabeller. Flik 2 slår ihop variabler
  som tabellerna delar i en lista och visar övriga i egna listor. Koden får
  `table = c(...)`, och antalet celler visas per tabell.
* `pxr_get_labels()`, `pxr_get_codes()`, `pxr_get_all_labels()` och
  `pxr_get_all_codes()` tar flera tabell-id i `tab` och slår ihop värdena utan
  dubbletter. Tabeller som saknar variabeln hoppas över.

#### Ändrat

* `pxr_pxweb2_get_data()` med flera tabeller anropar varje tabell med de variabler
  och värden den har. **Bakåtinkompatibelt:** en tabell utan något av de valda
  värdena hoppas nu över med ett meddelande, och `on_all_values_invalid = "stop"` ger
  fel först när ingen tabell kan anropas. Variabler som en tabell saknar ger inte
  längre en varning, om någon annan tabell har dem.
* Samma tabell-id flera gånger hämtas en gång.

### 0.8.2 (2026-11-02)

#### Rättat

* Gränssnittet kan öppna tabeller vars id bara består av siffror, till exempel
  `10531` hos SSB. De gav tidigare felet "table id must be a non-empty string of
  length 1".

### 0.8.1 (2026-11-02)

#### Ändrat

* Flikarna "2. Välj variabler" och "3. Kod och data" visar en varning när urvalet
  är större än 1 miljon celler. Hämtningen stoppas inte.

### 0.8.0 (2026-11-02)

#### Nytt

* Rutan "R-kod" i fliken "3. Kod och data" har knappen "Välj alla värden". Den
  markerar alla värden i alla variabler utom värdevariablerna, så att de får `"*"`
  i koden. En vald indelning behålls, och koden listar då värdena i stället för
  `"*"`. Är "Hämta alltid senaste perioden" ikryssad tas krysset bort.

### 0.7.1 (2026-11-02)

#### Ändrat

* Kommentaren i koden i fliken "3. Kod och data" anger också namnet att använda
  som `provider`, till exempel
  `# Statistiska centralbyrån (SCB), Sverige (provider = "scb")`. För en egen
  webbplats anges adressen.

### 0.7.0 (2026-11-02)

#### Nytt

* `provider` är `"scb"` som standard i `pxr_get_all_tabs()`, `pxr_search_tabs()`,
  `pxr_get_labels()`, `pxr_get_codes()`, `pxr_get_all_labels()` och
  `pxr_get_all_codes()`. Utan `provider` namnger du övriga argument, till exempel
  `pxr_get_all_codes(tab = "TAB638")`. Befintlig kod fungerar som förut.
* Fliken "1. Hitta tabell" har en ruta som visar namnet att ange som `provider`
  för den valda leverantören. För en egen webbplats visas adressen.

#### Dokumentation

* Leverantörernas namn listas på hjälpsidorna, i README och i vignetten.

### 0.6.0 (2026-11-02)

#### Nytt

* Stöd för Danmarks Statistik med den nya leverantören `"dst"`. API:et är inte
  PxWeb, men alla exporterade funktioner fungerar på samma sätt, till exempel
  `pxr_pxweb2_get_data("FOLK1A", base_url = "dst")`. Språken är `"da"` och `"en"`.

#### Begränsningar hos Danmarks Statistik

* `pxr_pxweb2_get_data()`: API:et har inga kodlistor, så `include_aggregations`
  ger en varning och ignoreras.
* `pxweb2_gui()` och `pxweb2_gui_addin()`: "Välj indelning" finns inte.
  Gränssnittet säger det när leverantören väljs.
* Resultatet har alltid en kolumn med tabellinnehållet som heter `Indhold`, även
  med `lang = "en"`.

#### Rättat

* `pxr_pxweb2_get_data()`: enstaka tabeller i API v1 finns men kan inte levereras
  som JSON-stat av servern, till exempel en tabell hos Jordbruksverket. Då skrevs
  serverns html-sida ut följt av "Not Found", som om tabellen saknades. Nu säger
  felmeddelandet att tabellen finns och att felet ligger hos servern.

### 0.5.0 (2026-11-02)

#### Nytt

* Stöd för PxWeb API v1. Alla exporterade funktioner fungerar mot en webbplats
  med API v1. Versionen avgörs av adressen: `/v1/` (eller `/v0/`) i adressen är
  API v1, och adressen ska ha med språket, till exempel
  `https://statistik.konj.se/PxWeb/api/v1/sv/KonjBar/`. Tabell-id är tabellens
  sökväg, till exempel `"indikatorer/Indikatorm.px"`.
* Nya inbyggda leverantörer med API v1: `"statfin"`, `"fohm"`, `"sjv"`,
  `"tillvaxtanalys"`, `"konj"` och `"skolverket"`.
* En leverantör som har både API v1 och v2 används alltid med v2, även om du
  anger adressen till dess v1, och ett meddelande säger det. Det gäller SCB och
  SSB. För övriga webbplatser kontrolleras en gång per session om v2 finns, så
  en leverantör som går över till v2 följs automatiskt. Tabellerna har andra id
  i v2, så en sökväg från v1 ger ett fel som förklarar det.
* Gränssnittet kan använda API v1 och visar vad som inte finns där när en
  sådan webbplats är vald.

#### Begränsningar i API v1

* `pxr_pxweb2_get_data()`: API v1 har inga kodlistor. `include_aggregations`
  ger en varning och ignoreras, och kolumnen `kommun` är tom för DeSO-områden,
  också med en varning. `kommun_kod` är komplett, och RegSO-områden får båda.
* `pxr_get_all_tabs()` och `pxr_search_tabs()`: API v1 har ingen tabellista.
  Listan tas fram med webbplatsens sökning, och där den saknas läses alla mappar.
  På en stor webbplats kan det ta några minuter första gången i en session, och
  ett meddelande säger det när det börjar.
* `pxweb2_gui()` och `pxweb2_gui_addin()`: "Välj indelning" finns inte,
  avslutade tabeller går inte att skilja ut och perioder visas inte i
  tabellistan.
* Språk: API v1 har ingen lista över språk. För en webbplats utanför paketets
  leverantörslista erbjuder gränssnittet bara språket i adressen.

#### Rättat

* `pxr_pxweb2_get_data()` (även API v2): en eliminerad variabel (`NA` i `query`)
  räknades som om alla dess värden hämtades, vilket kunde ge felet
  "Variable missing from query". En eliminerad variabel som också skulle få en
  kodkolumn, till exempel regionen eller med `code_variables = "*"`, gav ett fel
  från `dplyr::all_of()`. Nu räknas den inte och får ingen kodkolumn.

### 0.4.0 (2026-11-01)

#### Nytt

* Fliken "3. Kod och data" har kryssrutan "R-kompatibla kolumnnamn med
  janitor::clean_names()". Koden får då en extra rad
  `dataset_df <- janitor::clean_names(dataset_df)`, och data som visas och
  laddas ner i gränssnittet får samma kolumnnamn. Paketet janitor ligger
  under Suggests. Förvalet styrs med `options(pxweb2rgui.clean_names = TRUE)`.

### 0.3.0 (2026-11-01)

#### Ändrat

* Paketet exporterar bara funktionerna som börjar med `pxr_` samt
  `pxweb2_gui()` och `pxweb2_gui_addin()`. Övriga `pxweb2_*`-funktioner är
  interna och har inga egna hjälpsidor. De nås vid behov med
  `pxweb2rguisamlat:::`.
* Vignetten `pxweb2r` beskriver bara de exporterade funktionerna.

### 0.2.0 (2026-11-01)

#### Nytt

* `pxr_get_all_tabs(provider, updated_since, lang)` listar alla tabeller hos
  en leverantör som en data frame med kolumnerna `tabellid`, `tab_name` och
  `updated`.
* `pxr_search_tabs(provider, updated_since, lang, search_str)` ger de tabeller
  vars namn innehåller någon av texterna i `search_str`, oavsett stora och
  små bokstäver.
* `updated_since` är ett datum som text, till exempel `"2026-05-12"`, och
  behåller tabeller som uppdaterats den dagen eller senare. `provider` kan
  vara id, namn eller URL.

### 0.1.0 (2026-11-01)

#### Nytt

* pxweb2r 0.0.0.9011 och pxweb2rgui 0.5.1 är sammanslagna till ett paket.
  Alla funktioner, hjälpsidor, exempel, tester och båda vignetterna finns med.
* Gränssnittet anropar paketets egna funktioner, så pxweb2r behöver inte vara
  installerat.
* Koden som gränssnittet skriver börjar med `library(pxweb2rguisamlat)`.
* Tabellistan cachas under paketets nya namn.

Tidigare versioner finns i NEWS.md i pxweb2r och pxweb2rgui.

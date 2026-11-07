# pxweb2rgui 0.23.0

## Nytt

* Gränssnittet: i flik 1 kan du visa bara tabeller som uppdaterats sedan ett visst
  datum. Fältet "Bara tabeller uppdaterade sedan" öppnar en kalender där du väljer år,
  månad och dag. Filtret fungerar tillsammans med sökningen och "Visa även avslutade
  tabeller", och "Rensa" visar alla tabeller igen. Tabeller utan uppdateringsdatum döljs
  när ett datum är valt.
* Gränssnittet: trädet i flik 1 visar när varje tabell senast uppdaterades, till exempel
  `[2026-04-16]` efter perioden, som i SCB:s statistikdatabas. Det gäller både API v1
  och v2.

# pxweb2rgui 0.22.0

## Ändrat

* **Bakåtinkompatibelt:** kodkolumner från `code_variables` och från rutorna i flik 3
  heter nu `<variabel>_code` i stället för `<variabel>_kod`, till exempel `region_code`.
  Kolumnen `kommun_kod` från uppdelningen av DeSO och RegSO heter nu `kommun_code`. Kod
  som använder de gamla namnen behöver ändras. Kolumnerna `var_kod` och `varde_kod` i
  `pxr_get_comments()` har kvar sina namn.
* Gränssnittet: listorna med värden i flik 2 fungerar som i SCB:s statistikdatabas.
  * Ett klick väljer ett värde och avmarkerar de andra. Tidigare lade ett klick till
    eller tog bort värdet.
  * Ctrl+klick (Cmd på Mac) lägger till ett värde eller tar bort det.
  * Shift+klick väljer ett intervall från det senast klickade värdet och avmarkerar
    övriga. Ctrl+Shift+klick lägger intervallet till urvalet.
  * Att dra med musen väljer också ett intervall, och avmarkerar övriga om du inte håller
    ner Ctrl.
  * Ett klick eller Shift+klick avmarkerar också värden som ett sökord döljer. Använd
    Ctrl+klick för att bygga ett urval över flera sökningar.

# pxweb2rgui 0.21.1

## Rättat

* `pxr_pxweb2_get_data()` med en tabell som bara har en variabel, som
  `Avverkning/JO0312_07.px` hos `"skogsstyrelsen"`: etiketter som `"2025"`, `"9999"` för
  senaste perioden, jokertecken och `from()`, `to()` och `range()` fungerar nu. Tidigare
  gick bara koder, och ett årtal gav ett felmeddelande som skyllde på servern. Det gäller
  också koden som gränssnittet skriver ut för sådana tabeller.
* `from()`, `to()` och `range()` tar nu också etiketter, till exempel `"from(2024)"` i en
  tabell där koderna för år är löpnummer. Ett värde som är en kod betyder alltid koden,
  och andra värden jämförs med etiketterna.
* `"9999"` fungerar för variabler som heter `Läsår` eller `Kalenderår` på webbplatser som
  inte markerar dem som tid, som `"csn"`.
* Felmeddelandet för ett värde som tabellen saknar visar nu vilka värden tabellen har.

# pxweb2rgui 0.21.0

## Nytt

* Nya inbyggda leverantörer med PxWeb API v1:
  * `"csn"`: Centrala studiestödsnämnden, studiestöd och studieskulder (bara svenska).
  * `"energimyndigheten"`: Energimyndigheten, energianvändning och energiförsörjning.
  * `"mcf"`: Myndigheten för civilt försvar (tidigare MSB), bränder och räddningstjänst.
    Det gamla namnet `"msb"` fungerar också som `provider` och `base_url`.
  * `"skogsstyrelsen"` och `"skogsstyrelsen_d"`: Skogsstyrelsens två databaser, skogsbruk
    och skogsstatistik. Databas D har egna tabeller, bland annat om utrikeshandel.
  * `"slu_skog"`: SLU Riksskogstaxeringen, skogstillstånd och skogsresurser.
* Socialstyrelsen och Medlingsinstitutet är inte med. Socialstyrelsen har ett eget API som
  inte är PxWeb, och för Medlingsinstitutet hittades inget API.

## Dokumentation

* README, båda vignetterna och hjälpsidorna beskriver de nya leverantörerna.
* Hjälpsidan för `pxr_get_all_tabs()` har ett exempel med `"slu_skog"`.

# pxweb2rgui 0.20.1

## Rättat

* `pxr_get_comments()` och fliken "4. Kommentarer" med PxWeb API v1: kommentarer på
  tabellen och på variabler som webbplatsen bara lämnar i px-formatet kommer nu med. Hos
  Nordic Statistics saknades till exempel alla nio kommentarer på tabellen WORK30, och hos
  Konjunkturinstitutet en på `indikatorer/Indikatorm.px`. Funktionen gör ett anrop mer per
  tabell. För dessa kommentarer anger `obligatorisk` också vad som måste visas.

# pxweb2rgui 0.20.0

## Ändrat

* **Bakåtinkompatibelt:** paketet heter nu `pxweb2rgui`, inte `pxweb2rguisamlat`. Använd
  `library(pxweb2rgui)` i stället för `library(pxweb2rguisamlat)`, och samma sak i
  `pxweb2rguisamlat::`. Koden från gränssnittet börjar med det nya namnet. Paketet ersätter
  det äldre pxweb2rgui 0.5.x, och tabellistan sparas i cachen för `pxweb2rgui`.
* Gränssnittet: märkningen "Obligatorisk" och fetstilen för sådana kommentarer är borta,
  så alla kommentarer i fliken "4. Kommentarer" ser likadana ut. Kolumnen `obligatorisk`
  i `pxr_get_comments()` finns kvar.
* Gränssnittets rubrik är nu "pxweb2rgui".

## Rättat

* `pxr_get_comments()`: en kommentar som leverantören anger med värdet `false` i
  `noteMandatory` får nu `obligatorisk = FALSE`. Tidigare blev den `TRUE`.

# pxweb2rgui 0.19.0

## Ändrat

* Gränssnittet: hämtat data i fliken "3. Kod och data" raderas så fort något ändras som
  påverkar vad som hämtas: valda tabeller, leverantör, språk, valda värden, indelning
  eller "Hämta alltid senaste perioden". En text säger att data behöver hämtas på nytt.
* Rutorna för kodkolumner och för "R-kompatibla kolumnnamn" ändrar tabellen direkt, åt
  båda hållen och utan ny hämtning. Nedladdningen ger det som visas, och koden följer
  rutorna som tidigare.

# pxweb2rgui 0.18.0

## Ändrat

* Gränssnittet: kryssrutan "Visa kommentarer för vald tabell" i fliken "2. Välj
  variabler" är ersatt av knappen "Kommentarer" i fliken "1. Hitta tabell". Knappen är
  gråtonad tills en eller flera tabeller är valda. Ett klick hämtar kommentarerna och
  öppnar fliken "4. Kommentarer".
* Fliken "4. Kommentarer" finns alltid, men är gråtonad och går inte att klicka på när
  den saknar innehåll.
* Kommentarerna raderas när valet av tabeller ändras eller töms, när leverantör eller
  språk byts och när appen avslutas. Klicka på knappen igen för att hämta dem för det
  nya valet.

# pxweb2rgui 0.17.0

## Nytt

* Gränssnittet: kryssrutan "Visa kommentarer för vald tabell" i fliken "2. Välj
  variabler" ger den nya fliken "4. Kommentarer" med kommentarerna från
  `pxr_get_comments()`. Med flera valda tabeller får varje tabell en egen ruta med
  tabell-id och tabellnamn, och obligatoriska kommentarer är märkta. Länkar i
  kommentarerna går att klicka på.

# pxweb2rgui 0.16.0

## Nytt

* `pxr_get_comments()` hämtar kommentarerna (fotnoterna) till en eller flera tabeller,
  en rad per kommentar, med vad kommentaren gäller (tabell, variabel eller värde) och
  om leverantören säger att den måste visas tillsammans med siffrorna. Den fungerar
  mot API v2, PxWeb API v1 och Danmarks Statistik.

## Rättat

* I PxWeb API v1 tar paketet självt reda på sökvägen till ett entydigt tabell-id utan
  mappar, med eller utan `.px` och med stora eller små bokstäver, innan webbplatsen
  tillfrågas. Webbplatserna behandlar sådana id olika: hos till exempel
  `"vgr_gransregional"` gick det tidigare inte utan `.px`. Kolumnen `tabellid` och koden
  från gränssnittet har fortfarande hela sökvägen.

# pxweb2rgui 0.15.1

## Ändrat

* I PxWeb API v1 ger ett tabell-id utan mappar ett fel som listar de fullständiga
  sökvägarna när tabeller i flera mappar har det namnet, i alla funktioner som tar ett
  tabell-id. Hos `"nordicstats"` gäller det till exempel `"EDUC11"`, som finns i två
  mappar med olika åldersgrupper och enheter. **Bakåtinkompatibelt:** tidigare valdes
  den första tabellen utan varning. Ange i stället sökvägen från kolumnen `tabellid` i
  `pxr_get_all_tabs()`. Entydiga id och fullständiga sökvägar fungerar som förut.

# pxweb2rgui 0.15.0

## Nytt

* Nordic Statistics (nordicstatistics.org), Nordregios statistikdatabas med statistik
  om alla nordiska länder, finns som leverantören `"nordicstats"`. Den använder PxWeb
  API v1 och finns bara på engelska.

# pxweb2rgui 0.14.0

## Ändrat

* `"9999"` med flera tabeller är varje tabells egen senaste period, i tabellens ordning
  och bara i dess tidsvariabel. **Bakåtinkompatibelt:** tidigare hämtades bara den
  senaste perioden bland alla tabeller, och tabeller utan den hoppades över.
* Gränssnittet: en enda tabell behåller alla sina rader, och namn som står flera gånger
  får koden efter sig. **Bakåtinkompatibelt:** "100+ år" i SCB:s TAB5557 är åter flera
  rader, och alla värden valda ger `"*"`.

## Rättat

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

# pxweb2rgui 0.13.1

## Ändrat

* Flera tabeller: en tom lista, eller `NA` i `pxr_pxweb2_get_data()`, stoppar bara om
  alla tabeller kräver variabeln. Annars hoppas de tabeller som kräver den över, med
  ett meddelande, och övriga hämtas. I gränssnittet visas "(Obligatoriskt)" och den
  röda ramen bara när listan måste fyllas i.

## Rättat

* Gränssnittet: antalet celler i fliken "2. Välj variabler" uppdateras när valen ändras.

# pxweb2rgui 0.13.0

## Nytt

* Västra Götalandsregionens statistikdatabaser finns som leverantörer, en per databas:
  `"vgr_arbetsmarknad"`, `"vgr_demografi"`, `"vgr_folkhalsa"`, `"vgr_gransregional"`
  och `"vgr_personal"`. De använder PxWeb API v1.

## Rättat

* PxWeb API v1: en tabell där webbplatsen inte markerat tidsvariabeln behandlas ändå
  rätt om variabeln har ett vanligt namn, som tid, år eller månad. Då fungerar `"9999"`
  för senaste perioden, och gränssnittet ger förval och visar "Hämta alltid senaste
  perioden".
* Gränssnittet skrev varningen "Unknown or uninitialised column: `common`" i konsolen.

# pxweb2rgui 0.12.1

## Ändrat

* Gränssnittet med flera tabeller: kryssrutan "Visa gemensamma värden" finns nu för
  alla variabler, även obligatoriska.

# pxweb2rgui 0.12.0

## Nytt

* Gränssnittet med flera tabeller: frivilliga variabler som flera tabeller har får
  kryssrutan "Visa gemensamma värden" i flik 2. Den döljer och avmarkerar värden som
  inte finns i alla tabeller som har variabeln.

# pxweb2rgui 0.11.1

## Ändrat

* Gränssnittet med flera tabeller: en variabel som är obligatorisk i vissa tabeller
  och kan hoppas över i andra säger det, till exempel "Obligatorisk i TAB5557, kan
  hoppas över i TAB638."

# pxweb2rgui 0.11.0

## Ändrat

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

# pxweb2rgui 0.10.1

## Rättat

* `pxr_pxweb2_get_data()`: med en kodkolumn, till exempel `code_variables = "Alder"`,
  försvann talet ur klartexten, så att "20 år" blev "år". Koden tas nu bara bort för
  regioner och variabler där metadata säger att koden visas med värdet.

# pxweb2rgui 0.10.0

## Nytt

* Gränssnittet: "Välj indelning" finns nu också med flera tabeller. Indelningar med
  samma namn blir ett val även när id:na skiljer sig, och en indelning som bara vissa
  tabeller har visas med "(bara ...)".

## Rättat

* Gränssnittet: värden med samma namn men olika koder visas som en rad, och samma tal
  hämtas inte flera gånger. Det gäller till exempel åldrarna i SCB:s TAB5557 och
  tabellinnehåll med samma namn i flera tabeller. Är alla sådana värden valda listas
  koderna i koden i stället för `"*"`.

# pxweb2rgui 0.9.0

## Nytt

* Gränssnittet: Ctrl+klick i trädet väljer flera tabeller. Flik 2 slår ihop variabler
  som tabellerna delar i en lista och visar övriga i egna listor. Koden får
  `table = c(...)`, och antalet celler visas per tabell.
* `pxr_get_labels()`, `pxr_get_codes()`, `pxr_get_all_labels()` och
  `pxr_get_all_codes()` tar flera tabell-id i `tab` och slår ihop värdena utan
  dubbletter. Tabeller som saknar variabeln hoppas över.

## Ändrat

* `pxr_pxweb2_get_data()` med flera tabeller anropar varje tabell med de variabler
  och värden den har. **Bakåtinkompatibelt:** en tabell utan något av de valda
  värdena hoppas nu över med ett meddelande, och `on_all_values_invalid = "stop"` ger
  fel först när ingen tabell kan anropas. Variabler som en tabell saknar ger inte
  längre en varning, om någon annan tabell har dem.
* Samma tabell-id flera gånger hämtas en gång.

# pxweb2rgui 0.8.2

## Rättat

* Gränssnittet kan öppna tabeller vars id bara består av siffror, till exempel
  `10531` hos SSB. De gav tidigare felet "table id must be a non-empty string of
  length 1".

# pxweb2rgui 0.8.1

## Ändrat

* Flikarna "2. Välj variabler" och "3. Kod och data" visar en varning när urvalet
  är större än 1 miljon celler. Hämtningen stoppas inte.

# pxweb2rgui 0.8.0

## Nytt

* Rutan "R-kod" i fliken "3. Kod och data" har knappen "Välj alla värden". Den
  markerar alla värden i alla variabler utom värdevariablerna, så att de får `"*"`
  i koden. En vald indelning behålls, och koden listar då värdena i stället för
  `"*"`. Är "Hämta alltid senaste perioden" ikryssad tas krysset bort.

# pxweb2rgui 0.7.1

## Ändrat

* Kommentaren i koden i fliken "3. Kod och data" anger också namnet att använda
  som `provider`, till exempel
  `# Statistiska centralbyrån (SCB), Sverige (provider = "scb")`. För en egen
  webbplats anges adressen.

# pxweb2rgui 0.7.0

## Nytt

* `provider` är `"scb"` som standard i `pxr_get_all_tabs()`, `pxr_search_tabs()`,
  `pxr_get_labels()`, `pxr_get_codes()`, `pxr_get_all_labels()` och
  `pxr_get_all_codes()`. Utan `provider` namnger du övriga argument, till exempel
  `pxr_get_all_codes(tab = "TAB638")`. Befintlig kod fungerar som förut.
* Fliken "1. Hitta tabell" har en ruta som visar namnet att ange som `provider`
  för den valda leverantören. För en egen webbplats visas adressen.

## Dokumentation

* Leverantörernas namn listas på hjälpsidorna, i README och i vignetten.

# pxweb2rgui 0.6.0

## Nytt

* Stöd för Danmarks Statistik med den nya leverantören `"dst"`. API:et är inte
  PxWeb, men alla exporterade funktioner fungerar på samma sätt, till exempel
  `pxr_pxweb2_get_data("FOLK1A", base_url = "dst")`. Språken är `"da"` och `"en"`.

## Begränsningar hos Danmarks Statistik

* `pxr_pxweb2_get_data()`: API:et har inga kodlistor, så `include_aggregations`
  ger en varning och ignoreras.
* `pxweb2_gui()` och `pxweb2_gui_addin()`: "Välj indelning" finns inte.
  Gränssnittet säger det när leverantören väljs.
* Resultatet har alltid en kolumn med tabellinnehållet som heter `Indhold`, även
  med `lang = "en"`.

## Rättat

* `pxr_pxweb2_get_data()`: enstaka tabeller i API v1 finns men kan inte levereras
  som JSON-stat av servern, till exempel en tabell hos Jordbruksverket. Då skrevs
  serverns html-sida ut följt av "Not Found", som om tabellen saknades. Nu säger
  felmeddelandet att tabellen finns och att felet ligger hos servern.

# pxweb2rgui 0.5.0

## Nytt

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

## Begränsningar i API v1

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

## Rättat

* `pxr_pxweb2_get_data()` (även API v2): en eliminerad variabel (`NA` i `query`)
  räknades som om alla dess värden hämtades, vilket kunde ge felet
  "Variable missing from query". En eliminerad variabel som också skulle få en
  kodkolumn, till exempel regionen eller med `code_variables = "*"`, gav ett fel
  från `dplyr::all_of()`. Nu räknas den inte och får ingen kodkolumn.

# pxweb2rgui 0.4.0

## Nytt

* Fliken "3. Kod och data" har kryssrutan "R-kompatibla kolumnnamn med
  janitor::clean_names()". Koden får då en extra rad
  `dataset_df <- janitor::clean_names(dataset_df)`, och data som visas och
  laddas ner i gränssnittet får samma kolumnnamn. Paketet janitor ligger
  under Suggests. Förvalet styrs med `options(pxweb2rgui.clean_names = TRUE)`.

# pxweb2rgui 0.3.0

## Ändrat

* Paketet exporterar bara funktionerna som börjar med `pxr_` samt
  `pxweb2_gui()` och `pxweb2_gui_addin()`. Övriga `pxweb2_*`-funktioner är
  interna och har inga egna hjälpsidor. De nås vid behov med
  `pxweb2rguisamlat:::`.
* Vignetten `pxweb2r` beskriver bara de exporterade funktionerna.

# pxweb2rgui 0.2.0

## Nytt

* `pxr_get_all_tabs(provider, updated_since, lang)` listar alla tabeller hos
  en leverantör som en data frame med kolumnerna `tabellid`, `tab_name` och
  `updated`.
* `pxr_search_tabs(provider, updated_since, lang, search_str)` ger de tabeller
  vars namn innehåller någon av texterna i `search_str`, oavsett stora och
  små bokstäver.
* `updated_since` är ett datum som text, till exempel `"2026-05-12"`, och
  behåller tabeller som uppdaterats den dagen eller senare. `provider` kan
  vara id, namn eller URL.

# pxweb2rgui 0.1.0

## Nytt

* pxweb2r 0.0.0.9011 och pxweb2rgui 0.5.1 är sammanslagna till ett paket.
  Alla funktioner, hjälpsidor, exempel, tester och båda vignetterna finns med.
* Gränssnittet anropar paketets egna funktioner, så pxweb2r behöver inte vara
  installerat.
* Koden som gränssnittet skriver börjar med `library(pxweb2rguisamlat)`.
* Tabellistan cachas under paketets nya namn.

Tidigare versioner finns i NEWS.md i pxweb2r och pxweb2rgui.

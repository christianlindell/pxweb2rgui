# Två tabeller som delar Region och Tid, med olika år. A har också Kon, B har Alder.
ft_meta <- function(id) {
  dims <- function(codes, label = codes, extra = list()) {
    c(list(label = extra$label, category = list(
      index = as.list(stats::setNames(seq_along(codes) - 1, codes)),
      label = as.list(stats::setNames(label, codes))
    ), extension = list(elimination = isTRUE(extra$elimination))))
  }
  region <- dims(c("00", "20"), c("Riket", "Dalarna"), list(label = "region", elimination = TRUE))
  innehall <- dims("N1", "Antal", list(label = "tabellinnehåll"))
  if (id == "A") {
    dimension <- list(Region = region,
                      Kon = dims(c("1", "2"), c("män", "kvinnor"), list(label = "kön", elimination = TRUE)),
                      ContentsCode = innehall,
                      Tid = dims(c("2023", "2024"), extra = list(label = "år")))
  } else {
    dimension <- list(Region = region,
                      Alder = dims(c("0", "1"), extra = list(label = "ålder", elimination = TRUE)),
                      ContentsCode = innehall,
                      Tid = dims("2025", extra = list(label = "år")))
  }
  list(label = paste("Tabell", id), id = as.list(names(dimension)), dimension = dimension,
       role = list(time = list("Tid"), metric = list("ContentsCode"), geo = list("Region")),
       extension = list(px = list(tableid = id)))
}

ft_url <- "https://statistikdatabasen.scb.se/api/v2/tables/"

# hämtar via pxr_pxweb2_get_data() med en attrapp per tabell; `anrop` får varje
# tabells fråga och kodvariabler
ft_hamta <- function(query, ..., env = parent.frame()) {
  anrop <- list()
  local_mocked_bindings(
    pxweb2_get_metadata = function(table_id, ...) ft_meta(table_id),
    pxr_pxweb2_get_data = function(table, query, code_variables = NULL, ...) {
      id <- table$extension$px$tableid
      anrop[[id]] <<- list(query = query, code_variables = code_variables)
      tibble::tibble(value = 1)
    }
  )
  res <- .pxweb2_get_multiple_tables(c("A", "B"), query = query, lang = "sv", base_url = ft_url, ...)
  list(data = res, anrop = anrop)
}

test_that("varje tabell anropas med de värden den har", {
  r <- ft_hamta(list(Region = "00", ContentsCode = "N1", Tid = c("2023", "2024", "2025")), quiet = TRUE)
  expect_identical(r$anrop$A$query$Tid, c("2023", "2024"))
  expect_identical(r$anrop$B$query$Tid, "2025")
  expect_identical(r$data$table_id, c("A", "B"))
})

test_that("variabler som en tabell saknar lämnas utanför dess anrop", {
  r <- ft_hamta(list(Region = "00", Kon = "1", Alder = "0", ContentsCode = "N1", Tid = "*"), quiet = TRUE)
  expect_identical(names(r$anrop$A$query), c("Region", "Kon", "ContentsCode", "Tid"))
  expect_identical(names(r$anrop$B$query), c("Region", "Alder", "ContentsCode", "Tid"))
})

test_that("en tabell utan de valda värdena anropas inte", {
  q <- list(Region = "00", ContentsCode = "N1", Tid = "2025")
  expect_message(r <- ft_hamta(q), "Table A is not fetched")
  expect_named(r$anrop, "B")
  expect_identical(r$data$table_id, "B")
})

test_that("en variabel utan giltiga värden utelämnas om tabellen kan utelämna den", {
  # Kon finns bara i A och Alder bara i B; A saknar värdet 9 och kan utelämna Kon
  q <- list(Region = "00", Kon = "9", ContentsCode = "N1", Tid = "*")
  expect_message(r <- ft_hamta(q), "Table A is fetched without Kon")
  expect_named(r$anrop, c("A", "B"))
  expect_true(is.na(r$anrop$A$query$Kon))
  expect_identical(r$anrop$A$query$Tid, "*")
  # en region utelämnas inte: det skulle ge hela landet
  expect_message(r <- ft_hamta(list(Region = "99", Tid = "2024", ContentsCode = "N1"), on_all_values_invalid = "null"),
                 "Table A is not fetched")
  expect_length(r$anrop, 0)
  # en variabel som tabellen kräver utelämnas inte heller
  expect_message(r <- ft_hamta(list(Tid = "2025", ContentsCode = "N1")), "Table A is not fetched")
  expect_named(r$anrop, "B")
})

test_that("värden kan vara klartext och variabler anges med namn", {
  r <- ft_hamta(list(region = "Dalarna", ContentsCode = "N1", `år` = "2024"), quiet = TRUE)
  expect_named(r$anrop, "A")
  expect_identical(r$anrop$A$query$region, "Dalarna")
})

test_that("on_all_values_invalid gäller först när ingen tabell kan anropas", {
  q <- list(ContentsCode = "N1", Tid = "1900")
  expect_error(ft_hamta(q, quiet = TRUE), "None of the tables")
  expect_null(ft_hamta(q, quiet = TRUE, on_all_values_invalid = "null")$data)
  # "*": tabellen anropas ändå, och pxr_pxweb2_get_data() ger alla värden
  r <- ft_hamta(q, quiet = TRUE, on_all_values_invalid = "*")
  expect_named(r$anrop, c("A", "B"))
  expect_identical(r$anrop$A$query$Tid, "1900")
})

test_that("det som ingen tabell har rapporteras", {
  expect_warning(ft_hamta(list(Finns = "x", Tid = "2024"), quiet = TRUE), "Unknown variables.*Finns")
  expect_message(ft_hamta(list(Tid = c("2024", "1900"))), "no table has: 1900")
})

test_that("kodvariabler och aggregeringar gäller bara tabeller som har variabeln", {
  r <- ft_hamta(list(Tid = "*"), code_variables = c("Kon", "Alder", "Region"), quiet = TRUE)
  expect_identical(r$anrop$A$code_variables, c("Kon", "Region"))
  expect_identical(r$anrop$B$code_variables, c("Alder", "Region"))
  vars <- pxweb2_get_variables(ft_meta("A"))
  expect_identical(.pxweb2_aggregations_for_table(c(Alder = "agg_x"), vars), "none")
  expect_identical(.pxweb2_aggregations_for_table(c(Kon = "agg_x"), vars), c(Kon = "agg_x"))
  expect_identical(.pxweb2_aggregations_for_table("all", vars), "all")
})

test_that("kolumner som bara vissa tabeller har blir NA, med en varning", {
  local_mocked_bindings(
    pxweb2_get_metadata = function(table_id, ...) ft_meta(table_id),
    pxr_pxweb2_get_data = function(table, query, ...) {
      id <- table$extension$px$tableid
      if (id == "A") tibble::tibble(`kön` = "män", value = 1) else tibble::tibble(`ålder` = "0", value = 2)
    }
  )
  expect_warning(d <- .pxweb2_get_multiple_tables(
    c("A", "B"), query = list(Kon = "1", Alder = "0", Tid = "*"), lang = "sv", base_url = ft_url, quiet = TRUE),
    "identical column structure")
  expect_identical(names(d), c("table_id", "kön", "value", "ålder"))
  expect_identical(d[["kön"]], c("män", NA))
  expect_identical(d[["ålder"]], c(NA, "0"))
})

test_that("samma tabell två gånger hämtas en gång", {
  anrop <- 0
  local_mocked_bindings(
    .pxweb2_get_multiple_tables = function(...) stop("ska inte anropas"),
    .pxweb2_get_config = function(...) stop("en tabell"),
    .pxweb2_resolve_lang = function(lang, base_url) "sv"
  )
  expect_error(pxr_pxweb2_get_data(c("A", "A"), base_url = ft_url), "en tabell")
})

test_that("pxr_get_codes() och pxr_get_labels() slår ihop tabellerna", {
  local_mocked_bindings(pxweb2_get_metadata = function(table_id, ...) ft_meta(table_id))
  expect_identical(pxr_get_codes(tab = c("A", "B"), var = "Tid"), c("2023", "2024", "2025"))
  expect_identical(pxr_get_codes(tab = c("A", "B"), var = "Region"), c("00", "20"))
  expect_identical(pxr_get_labels(tab = c("A", "B"), var = "Region"), c("Riket", "Dalarna"))
  # samma som unique(c(...)), med filter i varje tabell
  expect_identical(pxr_get_codes(tab = c("A", "B"), var = "Tid", ends_with = "5"), "2025")
  expect_identical(pxr_get_codes(tab = c("A", "B"), var = "Tid", starts_with = "2024", to_labels = TRUE), "2024")
  # variabeln saknas i någon tabell: bara de andra räknas
  expect_identical(pxr_get_codes(tab = c("A", "B"), var = "Kon"), c("1", "2"))
  expect_identical(pxr_get_labels(tab = c("A", "B"), var = "ålder"), c("0", "1"))
  expect_error(pxr_get_codes(tab = c("A", "B"), var = "Finns"), "any of the tables: A, B")
  expect_error(pxr_get_codes(tab = c("A", "B"), var = c("Kon", "Tid")), "var must be")
  # samma tabell två gånger är en tabell, med felet för en tabell
  expect_identical(pxr_get_codes(tab = c("A", "A"), var = "Tid"), c("2023", "2024"))
  expect_error(pxr_get_codes(tab = "A", var = "Alder"), "does not exist in the table")
})

test_that("pxr_get_all_*() har alla variabler från alla tabeller", {
  local_mocked_bindings(pxweb2_get_metadata = function(table_id, ...) ft_meta(table_id))
  koder <- pxr_get_all_codes(tab = c("A", "B"))
  expect_named(koder, c("Region", "Kon", "ContentsCode", "Tid", "Alder"))
  expect_identical(koder$Tid, c("2023", "2024", "2025"))
  expect_identical(koder$Alder, c("0", "1"))
  expect_identical(pxr_get_all_labels(tab = c("A", "B"))$Kon, c("män", "kvinnor"))
  expect_identical(pxr_get_all_codes(tab = "A"), pxr_get_all_codes(tab = c("A", "A")))
})

ft_tab <- function(id) {
  m <- ft_meta(id)
  list(id = id, title = m$label, meta = m, variables = as.data.frame(pxweb2_get_variables(m)),
       values = pxweb2_get_values(m, include_aggregations = "none",
                                  return_df_if_only_one_variable = FALSE, quiet = TRUE),
       codelists = NULL)
}

test_that("delade variabler blir en lista, övriga egna listor", {
  m <- px_merge_tables(list(ft_tab("A"), ft_tab("B")))
  expect_identical(m$variables$code, c("Region", "Kon", "ContentsCode", "Tid", "Alder"))
  expect_identical(m$values$Tid$code, c("2023", "2024", "2025"))
  expect_identical(m$variables$size[m$variables$code == "Tid"], 3L)
  expect_identical(m$var_tables$Tid, c("A", "B"))
  expect_identical(m$var_tables$Kon, "A")
  expect_identical(m$var_tables$Alder, "B")
  # förval: varje tabells senaste år, så att båda tabellerna ger data
  expect_identical(m$defaults$Tid, c("2024", "2025"))
  expect_identical(m$defaults$ContentsCode, "N1")
  expect_length(m$defaults$Kon, 0)
  # en tabell är oförändrad
  ett <- px_merge_tables(list(ft_tab("A")))
  expect_identical(ett$variables, ft_tab("A")$variables)
  expect_identical(ett$defaults$Tid, "2024")
})

test_that("indelningar med samma namn blir ett val, även med olika id", {
  a <- ft_tab("A"); b <- ft_tab("B")
  cl <- function(ids, labels) data.frame(variable = "Region", id = ids, label = labels, type = "Valueset")
  a$codelists <- cl(c("vs_RegionLän07", "vs_RegionRiket99", "vs_Bara"), c("Län", "Riket", "Bara A"))
  b$codelists <- cl(c("vs_CKM02Län", "vs_CKM01Riket"), c("län", "riket"))
  m <- px_merge_tables(list(a, b))
  expect_identical(m$codelists$id, c("vs_RegionLän07", "vs_RegionRiket99", "vs_Bara"))
  expect_identical(m$codelists$label, c("Län", "Riket", "Bara A (bara A)"))
  expect_identical(m$codelist_groups$Region$vs_RegionLän07, c("vs_RegionLän07", "vs_CKM02Län"))
  expect_identical(m$codelist_groups$Region$vs_Bara, "vs_Bara")
  # en tabell utan indelningar: de andras finns kvar, med tabellens namn
  b$codelists <- NULL
  expect_identical(px_merge_tables(list(a, b))$codelists$label,
                   c("Län (bara A)", "Riket (bara A)", "Bara A (bara A)"))
})

test_that("en tabell behåller alla rader, och namn som upprepas får koden", {
  # som TAB1194 hos SCB: FA1100 och FL1100 heter båda monetärt guld
  t <- ft_tab("A")
  t$values$Kon <- data.frame(code = c("FA1", "FL1", "FA2"), label = c("guld", "guld", "mynt"))
  m <- px_merge_tables(list(t))
  expect_identical(m$values$Kon$code, c("FA1", "FL1", "FA2"))
  expect_identical(m$values$Kon$label, c("guld (FA1)", "guld (FL1)", "mynt"))
  expect_length(m$aliases, 0)
  # alla valda är "*", och inga koder tappas
  expect_identical(px_expand_aliases(list(Kon = "*"), m$aliases)$Kon, "*")
})

test_that("olika koder med samma namn blir en rad bara om varje tabell har en kod", {
  a <- ft_tab("A"); b <- ft_tab("B")
  a$values$ContentsCode <- data.frame(code = "N1", label = "Folkmängd")
  b$values$ContentsCode <- data.frame(code = "M9", label = "Folkmängd")
  m <- px_merge_tables(list(a, b))
  expect_identical(m$values$ContentsCode$code, "N1")
  expect_identical(m$defaults$ContentsCode, "N1")
  q <- px_expand_aliases(list(ContentsCode = "N1", Tid = "*"), m$aliases)
  expect_identical(q$ContentsCode, c("N1", "M9"))
  expect_identical(q$Tid, "*")
  expect_identical(px_expand_aliases(list(ContentsCode = "*"), m$aliases)$ContentsCode, "*")
  expect_identical(px_expand_aliases(list(ContentsCode = "N1"), m$aliases, indelade = "ContentsCode")$ContentsCode, "N1")
  cells <- px_table_cells(list(a, b), list(Region = "00", Kon = NA, Alder = NA, ContentsCode = q$ContentsCode, Tid = "*"))
  expect_identical(cells, c(2, 1))

  # A har två koder med namnet: inga hopslagna rader, så att A inte får samma tal två gånger
  a$values$ContentsCode <- data.frame(code = c("T1", "T5"), label = c("totalt", "totalt"))
  b$values$ContentsCode <- data.frame(code = "T5", label = "totalt")
  m <- px_merge_tables(list(a, b))
  expect_identical(m$values$ContentsCode$code, c("T1", "T5"))
  expect_identical(m$values$ContentsCode$label, c("totalt (T1) (bara A)", "totalt (T5)"))
  expect_length(m$aliases$ContentsCode, 0)
  expect_identical(px_expand_aliases(list(ContentsCode = "T1"), m$aliases)$ContentsCode, "T1")

  # regioner slås inte ihop: två orter kan heta likadant
  a <- ft_tab("A"); b <- ft_tab("B")
  a$values$Region <- data.frame(code = "1201", label = "Bergen")
  b$values$Region <- data.frame(code = "4601", label = "Bergen")
  m <- px_merge_tables(list(a, b))
  expect_identical(m$values$Region$code, c("1201", "4601"))
  expect_identical(m$values$Region$label, c("Bergen (1201) (bara A)", "Bergen (4601) (bara B)"))
})

test_that("samma kod med olika namn väljs på namn", {
  # som månaderna hos Jordbruksverket: kod 1 är olika saker i de två tabellerna
  a <- ft_tab("A"); b <- ft_tab("B")
  a$values$ContentsCode <- data.frame(code = c("1", "2"), label = c("Offentlig", "Privat"))
  b$values$ContentsCode <- data.frame(code = c("1", "3"), label = c("Privat", "Offentlig"))
  a$values$Tid <- data.frame(code = c("0", "1"), label = c("2024M01", "2024M02"))
  b$values$Tid <- data.frame(code = c("0", "1"), label = c("2023M12", "2024M01"))
  m <- px_merge_tables(list(a, b))
  expect_identical(m$values$ContentsCode$code, c("Offentlig", "Privat"))
  expect_true(all(m$values$ContentsCode$common))
  expect_identical(m$values$Tid$code, c("2023M12", "2024M01", "2024M02"))
  expect_identical(m$values$Tid$common, c(FALSE, TRUE, FALSE))
  # förval: varje tabells sista period, med namn
  expect_setequal(m$defaults$Tid, c("2024M02", "2024M01"))
  expect_setequal(m$defaults$ContentsCode, c("Offentlig", "Privat"))

  # frågan ger rätt kod i varje tabell
  for_a <- .pxweb2_query_for_table(list(ContentsCode = "Offentlig", Tid = "2024M01"), a$variables, a$values)
  for_b <- .pxweb2_query_for_table(list(ContentsCode = "Offentlig", Tid = "2024M01"), b$variables, b$values)
  expect_identical(for_a$query$ContentsCode, "Offentlig")
  expect_identical(for_b$query$Tid, "2024M01")
  expect_equal(as.numeric(px_table_cells(list(a, b), list(
    Region = "00", Kon = NA, Alder = NA, ContentsCode = "Offentlig", Tid = "2024M01"))), c(1, 1))

  # en region med enstaka olika stavningar följer koden
  a <- ft_tab("A"); b <- ft_tab("B")
  a$values$Region <- data.frame(code = c("00", "20", "21"), label = c("Riket", "Dalarna", "Gävleborg"))
  b$values$Region <- data.frame(code = c("00", "20", "21"), label = c("Riket", "Dalarnas län", "Gävleborg"))
  r <- px_merge_tables(list(a, b))$values$Region
  expect_identical(r$code, c("00", "20", "21"))
  expect_identical(r$label[2], "Dalarna")
  expect_true(all(r$common))
  # har de flesta koder olika namn är koderna löpnummer, och då gäller namnen
  b$values$Region <- data.frame(code = c("00", "20", "21"), label = c("Oslo", "Bergen", "Riket"))
  expect_identical(px_merge_tables(list(a, b))$values$Region$code,
                   c("Riket", "Dalarna", "Gävleborg", "Oslo", "Bergen"))

  # namn som inte går att välja var för sig räknas (X har olika koder, så namnen gäller)
  a <- ft_tab("A"); b <- ft_tab("B")
  a$values$ContentsCode <- data.frame(code = c("1", "2"), label = c("X", "X"))
  b$values$ContentsCode <- data.frame(code = c("1", "3"), label = c("Y", "X"))
  expect_identical(px_merge_tables(list(a, b))$dropped$ContentsCode, 1L)
})

test_that("samma kod med olika formulering väljs på kod", {
  # som SNI2007 i SCB:s TAB1786 och TAB5837: samma koder, olika ordalydelse
  a <- ft_tab("A"); b <- ft_tab("B")
  a$values$ContentsCode <- data.frame(
    code = c("A-U+US", "A", "B+C"),
    label = c("Total", "företag inom jordbruk, skogsbruk och fiske",
              "tillverkningsindustri; gruvor och mineralutvinningsindustri"))
  b$values$ContentsCode <- data.frame(
    code = c("A", "B+C"),
    label = c("jordbruk, skogsbruk och fiske", "tillverkning och utvinning"))
  m <- px_merge_tables(list(a, b))
  v <- m$values$ContentsCode
  expect_identical(v$code, c("A-U+US", "A", "B+C"))
  expect_identical(v$label[2:3], a$values$ContentsCode$label[2:3])
  expect_identical(v$common, c(FALSE, TRUE, TRUE))
  expect_length(m$dropped, 0)

  # frågan innehåller koden, och båda tabellerna får den
  for_b <- .pxweb2_query_for_table(list(ContentsCode = "A", Tid = "*"), b$variables, b$values)
  expect_identical(for_b$query$ContentsCode, "A")
  cells <- px_table_cells(list(a, b), list(
    Region = "00", Kon = NA, Alder = NA, ContentsCode = "A", Tid = "*"))
  expect_true(all(cells > 0))
})

test_that("tiden sorteras på namn, inte på kod som text", {
  a <- ft_tab("A"); b <- ft_tab("B")
  a$values$Tid <- data.frame(code = as.character(0:10), label = sprintf("2024M%02d", 1:11))
  b$values$Tid <- a$values$Tid
  m <- px_merge_tables(list(a, b))
  expect_identical(m$values$Tid$label, sprintf("2024M%02d", 1:11))
  expect_identical(m$defaults$Tid, "10")
})

test_that("9999 är varje tabells senaste period och bara i tidsvariabeln", {
  a <- ft_tab("A"); b <- ft_tab("B")
  va <- a$variables; vb <- b$variables
  # varje tabell behåller 9999, och dess eget anrop väljer perioden i tabellens ordning
  r <- ft_hamta(list(ContentsCode = "N1", Tid = "9999"), quiet = TRUE)
  expect_named(r$anrop, c("A", "B"))
  expect_identical(r$anrop$A$query$Tid, "9999")
  expect_identical(r$anrop$B$query$Tid, "9999")
  # blandat med andra värden: värde för värde
  r <- ft_hamta(list(ContentsCode = "N1", Tid = c("2023", "9999")), quiet = TRUE)
  expect_identical(r$anrop$A$query$Tid, c("2023", "9999"))
  expect_identical(r$anrop$B$query$Tid, "9999")
  # i en annan variabel är det en vanlig kod
  a$values$Kon <- data.frame(code = c("9999", "1"), label = c("okänt", "män"))
  expect_identical(.pxweb2_query_for_table(list(Kon = "9999"), va, a$values, latest_period_code = "9999")$query$Kon, "9999")
  x <- .pxweb2_query_for_table(list(Region = "9999"), va, a$values, latest_period_code = "9999")
  expect_identical(x$no_match, "Region")
  # ingen falsk varning om variabler från en tabell som hoppas över
  expect_no_warning(ft_hamta(list(Kon = "1", ContentsCode = "N1", Tid = "2025"), quiet = TRUE))
  # samma i appens räkning
  q <- list(Region = "*", Kon = NA, Alder = NA, ContentsCode = "N1", Tid = "9999")
  expect_identical(as.numeric(px_table_cells(list(a, b), q)), c(2, 2))
})

test_that("en tidsvariabel med annan kod påverkar inte andra tabeller", {
  # en tabell utan tidsvariabel stoppar inte de andra
  a <- ft_tab("A"); b <- ft_tab("B")
  b$variables$role[b$variables$code == "Tid"] <- "other"
  x <- .pxweb2_query_for_table(list(Tid = "9999"), b$variables, b$values, latest_period_code = "9999")
  expect_identical(x$no_match, "Tid")
  expect_identical(as.numeric(px_table_cells(list(a, b), list(
    Region = "00", Kon = NA, Alder = NA, ContentsCode = "N1", Tid = "9999"))), c(1, 0))
})

test_that("en variabels kod går före en annan variabels namn", {
  v <- data.frame(code = c("KOK", "Tid"), label = c("region", "år"), size = 2L,
                  elimination = c(TRUE, FALSE), role = c("geo", "time"), stringsAsFactors = FALSE)
  vals <- list(KOK = data.frame(code = c("0301", "3301"), label = c("Oslo", "Drammen")),
               Tid = data.frame(code = "2024", label = "2024"))
  # båda namnen i frågan: Region är en annan tabells kod
  x <- .pxweb2_query_for_table(list(KOK = "0301", Region = "3301", Tid = "2024"), v, vals)
  expect_identical(x$query$KOK, "0301")
  expect_false("Region" %in% names(x$query))
  expect_true("Region" %in% x$known_names)
  x <- .pxweb2_query_for_table(list(KOK = "0301", Region = NA, Tid = "2024"), v, vals)
  expect_identical(x$query$KOK, "0301")
  expect_length(x$no_match, 0)
  # bara namnet: det fungerar som förut
  x <- .pxweb2_query_for_table(list(region = "0301", Tid = "2024"), v, vals)
  expect_identical(x$query$region, "0301")
})

test_that("jokertecken och from(), to(), range() fungerar med flera tabeller", {
  r <- ft_hamta(list(ContentsCode = "N1", Tid = "from(2024)"), quiet = TRUE)
  expect_named(r$anrop, "A")
  expect_identical(r$anrop$A$query$Tid, "2024")
  r <- ft_hamta(list(ContentsCode = "N1", Tid = "range(2023,2024)"), quiet = TRUE)
  expect_identical(r$anrop$A$query$Tid, c("2023", "2024"))
  r <- ft_hamta(list(ContentsCode = "N1", Tid = "202*"), quiet = TRUE)
  expect_identical(r$anrop$A$query$Tid, c("2023", "2024"))
  expect_identical(r$anrop$B$query$Tid, "2025")
  r <- ft_hamta(list(Region = "2*", Kon = "1*", ContentsCode = "N1", Tid = "*"), quiet = TRUE)
  expect_identical(r$anrop$A$query$Region, "20")
  expect_identical(r$anrop$A$query$Kon, "1")
  # inget värde fångas: inte hela landet
  r <- ft_hamta(list(Region = "9*", ContentsCode = "N1", Tid = "*"), quiet = TRUE, on_all_values_invalid = "null")
  expect_length(r$anrop, 0)
})

test_that("en aggregering som tabellen saknar hämtar inte allt", {
  a <- ft_tab("A")
  x <- .pxweb2_query_for_table(list(Region = "agg_X"), a$variables, a$values, codelist_ids = character())
  expect_identical(x$no_match, "Region")
  x <- .pxweb2_query_for_table(list(Region = "agg_X"), a$variables, a$values, codelist_ids = "agg_X")
  expect_length(x$no_match, 0)
  expect_identical(x$query$Region, "agg_X")
  x <- .pxweb2_query_for_table(list(Kon = "agg_X"), a$variables, a$values, codelist_ids = character())
  expect_identical(x$left_out, "Kon")
  # en vanlig "*" är kvar
  expect_identical(.pxweb2_query_for_table(list(Region = "*"), a$variables, a$values, codelist_ids = character())$query$Region, "*")
})

test_that("NA, NULL och klartext hanteras lika i alla lägen", {
  a <- ft_tab("A")
  # NA i en obligatorisk variabel: tabellen anropas inte, även med "*"
  x <- .pxweb2_query_for_table(list(ContentsCode = NA), a$variables, a$values, eliminate = FALSE)
  expect_identical(x$required_na, "ContentsCode")
  r <- ft_hamta(list(ContentsCode = NA, Tid = "*"), quiet = TRUE, on_all_values_invalid = "*")
  expect_length(r$anrop, 0)
  # NULL är som NA
  x <- .pxweb2_query_for_table(list(Kon = NULL), a$variables, a$values)
  expect_true(is.na(x$query$Kon))
  # klartext avstängd: ett namn är inget giltigt värde
  x <- .pxweb2_query_for_table(list(Region = "Dalarna"), a$variables, a$values, allow_label_values = FALSE)
  expect_identical(x$no_match, "Region")
  x <- .pxweb2_query_for_table(list(Region = "Dalarna"), a$variables, a$values)
  expect_identical(x$query$Region, "Dalarna")
  # samma matchning i appens räkning
  expect_identical(.pxweb2_match_values(c("20", "Dalarna", "x"), a$values$Region), c(TRUE, TRUE, FALSE))
  expect_identical(.pxweb2_match_values(c("20", "Dalarna"), a$values$Region, FALSE), c(TRUE, FALSE))
})

test_that("svar 429 utan Retry-After väntar och försöker igen", {
  n <- 0
  svar <- function(status) structure(list(status_code = status, headers = structure(list(), class = c("insensitive", "list"))),
                                     class = "response")
  local_mocked_bindings(GET = function(...) { n <<- n + 1; svar(if (n < 2) 429L else 200L) }, .package = "httr")
  local_mocked_bindings(POST = function(...) { n <<- n + 1; svar(if (n < 2) 429L else 200L) }, .package = "httr")
  expect_identical(httr::status_code(.pxweb2_GET("https://x.example/", retry_wait_default = 0)), 200L)
  expect_identical(n, 2)
  n <- 0
  expect_identical(httr::status_code(.pxweb2_POST("https://x.example/", retry_wait_default = 0)), 200L)
  expect_identical(n, 2)
})

test_that("en tabell som inte går att läsa ändrar inget i valet", {
  b <- mock_backend()
  old <- options(pxweb2rgui.backend = b, pxweb2rgui.cache_file = tempfile(fileext = ".rds"))
  on.exit(options(old))
  testServer(px_app_server(), {
    session$setInputs(sprak = "sv")
    session$setInputs(vald_tabell = "TAB6104")
    t <- tabell()
    indelning(list(Region = list(id = "vs_x", type = "Valueset", values = data.frame(code = "1", label = "a"))))
    session$setInputs(vald_tabell = c("TAB6104", "TAB999"))
    expect_identical(tabell()$id, "TAB6104")
    expect_identical(tabell()$prefix, t$prefix)
    expect_identical(valda(), "TAB6104")
    expect_named(indelning(), "Region")
    session$setInputs(vald_tabell = c("TAB6104", "TAB5444"))
    expect_identical(valda(), c("TAB6104", "TAB5444"))
    expect_length(indelning(), 0)
  })
  expect_match(px_tree_js, "pxSetTables", fixed = TRUE)
})

test_that("en indelning räknas med koderna i de tabeller som har den", {
  tabs <- list(ft_tab("A"), ft_tab("B"))
  q <- list(Region = "agg1", Kon = NA, Alder = NA, ContentsCode = "N1", Tid = "*")
  # bara A har aggregeringen, och B har inte koden
  expect_identical(as.numeric(px_table_cells(tabs, q, indelade = list(Region = list(A = c("agg1", "agg2"))))), c(2, 0))
  expect_identical(px_table_cells(tabs, q, indelade = list(Region = list(A = "agg1", B = "agg1"))), c(2, 1))
})

test_that("varje tabell får bara sina egna aggregeringar", {
  m <- ft_meta("A")
  m$dimension$Region$extension$codelists <- list(list(id = "agg_a", type = "Aggregation"))
  vars <- pxweb2_get_variables(m)
  ia <- c(Region = "agg_a", Region = "agg_b")
  expect_identical(.pxweb2_aggregations_for_table(ia, vars, m), c(Region = "agg_a"))
  expect_identical(.pxweb2_aggregations_for_table(c(Region = "agg_b"), vars, m), "none")
  expect_identical(.pxweb2_aggregations_for_table(c(Region = "all"), vars, m), c(Region = "all"))
})

test_that("appens cellräkning utelämnar samma variabler", {
  tabs <- list(ft_tab("A"), ft_tab("B"))
  q <- list(Region = "00", Kon = "9", Alder = "0", ContentsCode = "N1", Tid = "*")
  cells <- px_table_cells(tabs, q)
  expect_equal(as.numeric(cells), c(1 * 1 * 1 * 2, 1 * 1 * 1 * 1))
  expect_identical(attr(cells, "left_out"), list(A = "kön"))
  # region: tabellen hämtas inte, och attributet säger varför
  q$Region <- "99"
  cells <- px_table_cells(tabs, q)
  expect_equal(as.numeric(cells), c(0, 0))
  expect_identical(attr(cells, "missing"), list(A = "region", B = "region"))
  # alla variabler som saknas nämns
  q$Tid <- "1900"
  expect_identical(attr(px_table_cells(tabs, q), "missing")$A, c("region", "år"))
})

test_that("koden säger vad som händer med tabeller som saknar valda värden", {
  notes <- px_table_notes(list(`a/A.px` = c("kön", "ålder")), list(B = "år"))
  expect_identical(notes, c("A.px hämtas utan kön, ålder: saknar de valda värdena, så raderna gäller alla värden",
                            "B hämtas inte: inga giltiga värden valda för år"))
  expect_length(px_table_notes(), 0)
  code <- px_query_code(c("A", "B"), c("Ta", "Tb"), list(Tid = "2023"), notes = notes)
  expect_match(code, "# Tb\n# A.px hämtas utan", fixed = TRUE)
  expect_identical(length(parse(text = code)), 2L)
})

test_that("en tibble utan kolumnen common ger ingen varning", {
  v <- tibble::tibble(code = c("1", "2"), label = c("a", "b"))
  expect_null(px_common_of(v))
  expect_no_warning(px_var_box("x", "kön", v, "other", TRUE))
  expect_identical(px_common_of(data.frame(common = TRUE)), TRUE)
})

test_that("kryssrutan Visa gemensamma värden finns bara där den ska", {
  # märkningen av värden: bara de som saknas i någon tabell får data-bara
  html <- px_options_html(c("1", "T"), c("män", "totalt"), common = c(TRUE, FALSE))
  expect_match(html, 'data-bara="1"', fixed = TRUE)
  expect_identical(lengths(regmatches(html, gregexpr("data-bara", html))), 1L)
  expect_false(grepl("data-bara", px_options_html("1", "män"), fixed = TRUE))
  # px_mark_common() gör samma sak för en vald indelning
  v <- data.frame(code = c("a", "b"), label = c("A", "B"))
  m <- px_mark_common(v, list(`x/T1.px` = c("a", "b"), T2 = "a"))
  expect_identical(m$common, c(TRUE, FALSE))
  expect_identical(m$label, c("A", "B (bara T1.px)"))
  expect_identical(px_mark_common(v, list(T1 = "a")), v)

  b <- mock_backend()
  gm <- b$get_metadata
  b$get_metadata <- function(table_id, ...) {
    m <- gm(table_id)
    if (table_id == "TAB5444") {
      m$dimension$Kon <- NULL
      m$id <- as.list(names(m$dimension))
    }
    m
  }
  old <- options(pxweb2rgui.backend = b, pxweb2rgui.cache_file = tempfile(fileext = ".rds"))
  on.exit(options(old))
  testServer(px_app_server(), {
    session$setInputs(sprak = "sv")
    session$setInputs(vald_tabell = c("TAB6104", "TAB5444"))
    t <- tabell()
    html <- as.character(output$variabler$html)
    # alla variabler, även obligatoriska och de som bara en tabell har
    expect_identical(lengths(regmatches(html, gregexpr("px-common-only", html))), nrow(t$variables))
    for (i in seq_len(nrow(t$variables))) {
      expect_match(html, paste0('px-common-only" data-target="', t$prefix, "varden_", i), fixed = TRUE)
    }
    # en vald tabell: ingen kryssruta
    session$setInputs(vald_tabell = "TAB6104")
    expect_false(grepl("px-common-only", as.character(output$variabler$html), fixed = TRUE))
  })
})

test_that("det syns om en variabel kan hoppas över i bara vissa tabeller", {
  expect_identical(px_elimination_note(c(`a/A.px` = FALSE, B = TRUE)),
                   "Obligatorisk i A.px, kan hoppas över i B. Utan val hämtas inte A.px.")
  expect_identical(px_elimination_note(c(A = FALSE, B = TRUE, C = TRUE)),
                   "Obligatorisk i A, kan hoppas över i B, C. Utan val hämtas inte A.")
  expect_null(px_elimination_note(c(A = TRUE, B = TRUE)))
  # krävs i alla valda tabeller: rubriken "(Obligatoriskt)" räcker
  expect_null(px_elimination_note(c(A = FALSE, B = FALSE)))
  expect_null(px_elimination_note(c(A = TRUE), n_tables = 2))
  # krävs i en av två tabeller, och den andra saknar variabeln
  expect_identical(px_elimination_note(c(A = FALSE), n_tables = 2),
                   "Obligatorisk i A. Utan val hämtas inte A.")

  a <- ft_tab("A"); b <- ft_tab("B")
  b$variables$elimination[b$variables$code == "Region"] <- FALSE
  m <- px_merge_tables(list(a, b))
  expect_identical(m$var_elimination$Region, c(A = TRUE, B = FALSE))
  expect_identical(px_elimination_note(m$var_elimination$Region),
                   "Obligatorisk i B, kan hoppas över i A. Utan val hämtas inte B.")
  # tom lista stoppar inte: A kan hämtas
  expect_true(m$variables$elimination[m$variables$code == "Region"])
  expect_identical(m$var_elimination$Kon, c(A = TRUE))
  # krävs i alla tabeller: obligatorisk, och en variabel som en tabell saknar är det inte
  expect_false(m$variables$elimination[m$variables$code == "Tid"])
  a$variables$elimination[a$variables$code == "Kon"] <- FALSE
  expect_true(px_merge_tables(list(a, b))$variables$elimination[m$variables$code == "Kon"])
})

test_that("en tom lista hoppar över tabeller som kräver variabeln", {
  a <- ft_tab("A"); b <- ft_tab("B")
  b$variables$elimination[b$variables$code == "Region"] <- FALSE
  q <- list(Region = NA, Kon = NA, Alder = NA, ContentsCode = "N1", Tid = "*")
  cells <- px_table_cells(list(a, b), q)
  expect_equal(as.numeric(cells), c(2, 0))
  expect_identical(attr(cells, "missing"), list(B = "region"))
  expect_identical(px_table_notes(missing = attr(cells, "missing")),
                   "B hämtas inte: inga giltiga värden valda för region")

  # samma regel i pxr_pxweb2_get_data()
  vars <- pxweb2_get_variables(ft_meta("A"))
  vals <- pxweb2_get_values(ft_meta("A"), include_aggregations = "none",
                            return_df_if_only_one_variable = FALSE, quiet = TRUE)
  for_a <- .pxweb2_query_for_table(list(Region = NA, ContentsCode = NA, Tid = "*"), vars, vals)
  expect_identical(for_a$required_na, "ContentsCode")
  expect_length(.pxweb2_query_for_table(list(Region = NA, ContentsCode = "N1", Tid = "*"), vars, vals)$required_na, 0)
})

test_that("värden som bara vissa tabeller har säger vilka", {
  a <- ft_tab("A"); b <- ft_tab("B")
  m <- px_merge_tables(list(a, b))
  expect_identical(m$values$Tid$label, c("2023 (bara A)", "2024 (bara A)", "2025 (bara B)"))
  # finns i båda: ingen märkning, och koden påverkas inte
  expect_identical(m$values$Region$label, c("Riket", "Dalarna"))
  expect_identical(m$values$Tid$code, c("2023", "2024", "2025"))
  # en variabel som bara en tabell har märks inte per värde
  expect_identical(m$values$Kon$label, c("män", "kvinnor"))
  # samma namn med olika koder räknas som samma värde
  a$values$ContentsCode <- data.frame(code = "N1", label = "Folkmängd")
  b$values$ContentsCode <- data.frame(code = c("M9", "M8"), label = c("Folkmängd", "Folkökning"))
  m <- px_merge_tables(list(a, b))
  expect_identical(m$values$ContentsCode$label, c("Folkmängd", "Folkökning (bara B)"))
})

test_that("cellerna räknas per tabell", {
  tabs <- list(ft_tab("A"), ft_tab("B"))
  q <- list(Region = "*", Kon = NA, ContentsCode = "N1", Tid = c("2023", "2024", "2025"), Alder = "*")
  expect_identical(px_table_cells(tabs, q), c(2 * 1 * 1 * 2, 2 * 2 * 1 * 1))
  # bara tabell B har 2025
  q$Tid <- "2025"
  expect_identical(as.numeric(px_table_cells(tabs, q)), c(0, 4))
  # senaste perioden är varje tabells egen
  q$Tid <- "9999"
  expect_identical(as.numeric(px_table_cells(tabs, q)), c(2, 4))
})

test_that("koden har alla tabeller och titlar", {
  code <- px_query_code(c("A", "B"), c("Tabell A", "Tabell B"), list(Tid = c("2023", "2025")))
  expect_match(code, 'table = c("A", "B")', fixed = TRUE)
  expect_match(code, "# Tabell A\n# Tabell B\n", fixed = TRUE)
  expect_identical(length(parse(text = code)), 2L)
  expect_match(px_query_code("A", "Tabell A", list(Tid = "2023")), 'table = "A"', fixed = TRUE)
  expect_identical(px_file_id(c("A", "B")), "A_m_fl")
  expect_identical(px_file_id("A"), "A")
})

test_that("gränssnittet hanterar flera valda tabeller", {
  # TAB6104 har år och TAB5444 månader; bara kön delar de inte på något sätt
  b <- mock_backend()
  gd <- NULL
  b$get_data <- function(table, ...) { gd <<- table; tibble::tibble(x = 1) }
  old <- options(pxweb2rgui.backend = b, pxweb2rgui.cache_file = tempfile(fileext = ".rds"))
  on.exit(options(old))
  testServer(px_app_server(), {
    session$setInputs(sprak = "sv")
    session$setInputs(vald_tabell = c("TAB6104", "TAB5444"))
    t <- tabell()
    expect_identical(t$id, c("TAB6104", "TAB5444"))
    expect_identical(valda(), c("TAB6104", "TAB5444"))
    tid <- t$values$Tid$code
    expect_true(all(c("2024", "2025M12") %in% tid))
    expect_identical(t$variables$label[t$variables$code == "Tid"], "år / månad")
    expect_match(fraga()$code, 'table = c("TAB6104", "TAB5444")', fixed = TRUE)
    expect_match(fraga()$code, "# Testtabell TAB6104\n# Testtabell TAB5444\n", fixed = TRUE)
    # förval: första tabellinnehållet och varje tabells senaste period, en cell var
    expect_equal(unname(fraga()$table_cells), c(1, 1))
    expect_match(cell_text(fraga()), "Per tabell: TAB6104: 1; TAB5444: 1", fixed = TRUE)

    # flera tabeller hämtas med sina id:n, en tabell med sin metadata
    session$setInputs(hamta_data = 1)
    expect_identical(gd, c("TAB6104", "TAB5444"))

    # ett år som bara den ena tabellen har
    i <- match("Tid", t$variables$code)
    args <- list(c("2024"))
    names(args) <- paste0(t$prefix, "varden_", i)
    do.call(session$setInputs, args)
    expect_equal(unname(fraga()$table_cells), c(1, 0))
    expect_match(cell_text(fraga()), "TAB5444: hämtas inte", fixed = TRUE)

    # att ta bort alla tabeller nollställer valet
    session$setInputs(vald_tabell = "")
    expect_null(tabell())
    expect_identical(valda(), character())

    session$setInputs(vald_tabell = "TAB6104")
    session$setInputs(hamta_data = 2)
    expect_true(is.list(gd))
  })
})

test_that("flik 2 visar delade och egna variabler var för sig", {
  b <- mock_backend()
  gm <- b$get_metadata
  b$get_metadata <- function(table_id, ...) {
    m <- gm(table_id)
    if (table_id == "TAB5444") {
      m$dimension$Kon <- NULL
      m$id <- as.list(names(m$dimension))
    }
    m
  }
  old <- options(pxweb2rgui.backend = b, pxweb2rgui.cache_file = tempfile(fileext = ".rds"))
  on.exit(options(old))
  testServer(px_app_server(), {
    session$setInputs(sprak = "sv")
    session$setInputs(vald_tabell = c("TAB6104", "TAB5444"))
    html <- as.character(output$variabler$html)
    expect_match(html, "Variabler som alla tabeller har", fixed = TRUE)
    expect_match(html, "Variabler som bara finns i vissa tabeller", fixed = TRUE)
    expect_match(html, "Finns i: TAB6104", fixed = TRUE)
    # kön kan hoppas över i TAB6104 men inte i TAB4718
    session$setInputs(vald_tabell = c("TAB6104", "TAB4718"))
    html <- as.character(output$variabler$html)
    expect_match(html, "Obligatorisk i TAB4718, kan hoppas över i TAB6104. Utan val hämtas inte TAB4718.", fixed = TRUE)
  })
})

test_that("trädet markerar alla valda tabeller", {
  tables <- data.frame(id = c("A", "B"), label = c("a", "b"), first_period = NA, last_period = NA,
                       discontinued = FALSE)
  paths <- data.frame(table_id = c("A", "B"))
  html <- as.character(px_tree_html(list(key = px_root_key, children = list(), tables = c("A", "B"), n = 2),
                                    tables, selected = c("A", "B")))
  expect_identical(lengths(regmatches(html, gregexpr("px-selected", html))), 2L)
  expect_match(px_tree_js, "e.ctrlKey", fixed = TRUE)
})

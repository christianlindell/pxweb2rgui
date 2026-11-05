# PxWeb API v1, utan nätverk: en påhittad webbplats med ett par mappar och en tabell

v1_url <- "https://v1.example/api/v1/sv/db/"

v1_response <- function(url, json, status = 200L) {
  structure(list(url = url, status_code = status,
                 headers = structure(list(`content-type` = "application/json; charset=utf-8"),
                                     class = c("insensitive", "list")),
                 content = charToRaw(enc2utf8(json))),
            class = "response")
}

v1_table <- '{"title":"Folkmängd","variables":[
  {"code":"Region","text":"region","values":["01","02","03"],"valueTexts":["Ett","Två","Tre"],"elimination":true},
  {"code":"ContentsCode","text":"tabellinnehåll","values":["N1"],"valueTexts":["Antal"]},
  {"code":"Tid","text":"år","values":["2022","2023","2024"],"valueTexts":["2022","2023","2024"],"time":true}]}'

v1_data <- '{"version":"2.0","class":"dataset","id":["Region","ContentsCode","Tid"],"size":[1,1,1],
  "dimension":{
  "Region":{"label":"region","category":{"index":{"01":0},"label":{"01":"Ett"}}},
  "ContentsCode":{"label":"tabellinnehåll","category":{"index":{"N1":0},"label":{"N1":"Antal"}}},
  "Tid":{"label":"år","category":{"index":{"2024":0},"label":{"2024":"2024"}}}},
  "value":[7]}'

# GET-svaren på webbplatsen; söktjänsten finns bara om search anges.
# Svaren inleds med en BOM, som hos riktiga v1-webbplatser.
v1_get <- function(calls = new.env(), search = NULL, fail = character()) {
  function(url, ..., query = NULL) {
    calls$urls <- c(calls$urls, url)
    json <- if (grepl("?config", url, fixed = TRUE)) {
      '{"maxValues":100,"maxCells":1000,"maxCalls":30,"timeWindow":10}'
    } else if (grepl("?query=", url, fixed = TRUE)) {
      search
    } else if (url %in% fail) {
      NULL
    } else if (grepl("/db/$", url)) {
      '[{"id":"A","type":"l","text":"Ämne A\\n"},{"id":"rubrik","type":"h","text":"Rubrik"},
        {"id":"T0.px","type":"t","text":"Tabell noll","updated":"2026-01-02T08:00:00"}]'
    } else if (grepl("/db/A/$", url)) {
      '[{"id":"B%20C","type":"l","text":"Under"}]'
    } else if (grepl("/db/A/B%2520C/$", url)) {
      '[{"id":"T1.px","type":"t","text":"Tabell ett","updated":"2026-06-01T08:00:00"}]'
    } else if (grepl("/T1\\.px$", url)) {
      v1_table
    }
    if (is.null(json)) return(v1_response(url, "{}", 400L))
    v1_response(url, paste0("﻿", json))
  }
}

test_that("a URL with /v1/ or /v0/ is API v1 and is normalised to its database level", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  local_mocked_bindings(.pxweb2_GET = v1_get())
  f <- pxweb2rgui:::.pxweb2_normalise_base_url
  expect_identical(f("https://x.example/api/v1/sv/db/?config"), "https://x.example/api/v1/sv/db/")
  expect_identical(f("https://x.example/api/v1/en"), "https://x.example/api/v1/en/")
  expect_identical(f("https://x.example/api/v0/no/table"), "https://x.example/api/v0/no/table/")
  expect_identical(f("https://x.example/api/v1/sv/Min%20databas"), "https://x.example/api/v1/sv/Min databas/")
  expect_error(f("https://x.example/api/v1/"), "include the language")
  expect_identical(f("https://x.example/api/v2"), "https://x.example/api/v2/tables/")
  expect_identical(pxweb2rgui:::px_tables_url("https://x.example/api/v1/sv/db"), "https://x.example/api/v1/sv/db/")
})

test_that("the built-in API v1 providers keep their URL and work by id", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  local_mocked_bindings(.pxweb2_GET = v1_get())
  p <- pxweb2_providers()
  expect_true(all(c("statfin", "fohm", "sjv", "tillvaxtanalys", "konj", "skolverket") %in% p$id))
  # Västra Götalandsregionen: en leverantör per databas, alla med API v1 på samma server
  vgr <- p[startsWith(p$id, "vgr_"), ]
  expect_identical(vgr$id, c("vgr_arbetsmarknad", "vgr_demografi", "vgr_folkhalsa",
                             "vgr_gransregional", "vgr_personal"))
  expect_true(all(startsWith(vgr$tables_url, "https://pxweb2022.vgregion.se/Pxwebb/api/v1/sv/")))
  expect_true(all(vapply(vgr$tables_url, .pxweb1_is_url, TRUE)))
  expect_identical(anyDuplicated(vgr$tables_url), 0L)
  expect_identical(.pxweb1_parts(vgr$tables_url[3])$lang, "sv")
  expect_true(all(vgr$default_lang == "sv"))
  expect_true(all(startsWith(vgr$name, "Västra Götalandsregionen: ")))
  expect_identical(pxweb2_base_url("konj"), "https://statistik.konj.se/PxWeb/api/v1/sv/KonjBar/")
  expect_identical(p$tables_url[p$id == "sjv"], p$api_url[p$id == "sjv"])
  expect_error(pxweb2_base_url("scb_v1"), "Unknown provider")
})

test_that("Nordic Statistics is a built-in API v1 provider in English", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  local_mocked_bindings(.pxweb2_GET = function(url, ...) v1_response(url, "{}", 404L))
  p <- pxweb2_providers()
  n <- p[p$id == "nordicstats", ]
  expect_identical(nrow(n), 1L)
  expect_identical(n$tables_url, "https://pxweb.nordicstatistics.org/api/v1/en/Nordic Statistics/")
  expect_true(.pxweb1_is_url(n$tables_url))
  expect_identical(.pxweb1_parts(n$tables_url)$lang, "en")
  expect_identical(n$default_lang, "en")
  expect_identical(n$languages, "en")
  expect_identical(pxweb2_base_url("NordicStats"), n$tables_url)
  expect_identical(pxweb2rgui:::.pxweb2_resolve_lang(NULL, n$tables_url), "en")
})

test_that("a provider with both versions is always used through API v2", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  f <- pxweb2rgui:::.pxweb2_normalise_base_url
  calls <- new.env()
  local_mocked_bindings(.pxweb2_GET = v1_get(calls))

  # v2 på en annan adress är känt utan något anrop, och sägs bara en gång
  expect_message(res <- f("https://api.scb.se/OV0104/v1/doris/sv/ssd"), "is used instead")
  expect_identical(res, "https://statistikdatabasen.scb.se/api/v2/tables/")
  expect_silent(f("https://api.scb.se/OV0104/v1/doris/en/ssd/"))
  expect_message(res <- f("https://data.ssb.no/api/v0/no/table/"), "is used instead")
  expect_identical(res, "https://data.ssb.no/api/pxwebapi/v2/tables/")
  expect_null(calls$urls)

  # en webbplats utan v2 frågas en gång per session
  expect_identical(f(v1_url), v1_url)
  expect_identical(f(v1_url), v1_url)
  expect_identical(calls$urls, "https://v1.example/api/v2/config")
})

test_that("a site that has moved to API v2 is followed", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  old <- options(pxweb2r.providers = data.frame(id = "flyttad", api_url = "https://flyttad.example/api/v1/sv/db/"))
  on.exit(options(old), add = TRUE)
  local_mocked_bindings(.pxweb2_GET = function(url, ...) {
    if (grepl("/api/v2/config$", url)) return(v1_response(url, '{"apiVersion":"2.3.2","maxDataCells":1000}'))
    v1_response(url, "{}", 404L)
  })
  expect_message(res <- pxweb2_base_url("flyttad"), "is used instead")
  expect_identical(res, "https://flyttad.example/api/v2/tables/")
  expect_identical(pxweb2rgui:::.pxweb1_latest_url(pxweb2_providers()$tables_url[pxweb2_providers()$id == "flyttad"]), res)
})

test_that("an answer that is not an API v2 does not count, nor does a failed call", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  local_mocked_bindings(.pxweb2_GET = function(url, ...) v1_response(url, '{"annat":1}'))
  expect_identical(pxweb2rgui:::.pxweb1_latest_url(v1_url), v1_url)
  pxweb2_clear_cache()
  local_mocked_bindings(.pxweb2_GET = function(url, ...) stop("no network"))
  expect_identical(pxweb2rgui:::.pxweb1_latest_url(v1_url), v1_url)
})

test_that("a v1 path at a provider with API v2 gives an error that explains it", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  expect_error(suppressMessages(pxweb2_get_metadata("BE/BE0101/BefolkningNy", base_url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/")),
               "API v2, which is always used")
})

test_that("addresses get the language asked for, with every folder encoded", {
  f <- pxweb2rgui:::.pxweb1_url
  expect_identical(f("https://x.example/v1/sv/Min db/"), "https://x.example/v1/sv/Min%20db/")
  expect_identical(f("https://x.example/v1/sv/db/", "en", "A/T.px"), "https://x.example/v1/en/db/A/T.px")
  expect_identical(f("https://x.example/v1/sv/", "en", "Lön/T", folder = TRUE), "https://x.example/v1/en/L%C3%B6n/T/")
})

test_that("languages come from the provider list and the URL", {
  l <- pxweb2rgui:::.pxweb1_languages("https://pxdata.stat.fi/PXWeb/api/v1/sv/StatFin/")
  expect_identical(l$id, c("fi", "sv", "en"))
  expect_identical(attr(l, "default"), "sv")
  # samma webbplats fast adressen har ett annat språk
  en <- pxweb2rgui:::.pxweb1_languages("https://pxdata.stat.fi/PXWeb/api/v1/en/StatFin/")
  expect_identical(attr(en, "default"), "en")
  expect_identical(en$id, c("fi", "sv", "en"))

  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  local_mocked_bindings(.pxweb2_GET = v1_get())
  res <- pxweb2rgui:::.pxweb2_resolve_lang
  expect_identical(res(NULL, v1_url), "sv")
  # okänd webbplats: språket provas i stället för att stoppas
  expect_identical(res("fi", v1_url), "fi")
  expect_error(res("de", pxweb2_base_url("statfin")), "not offered")
})

test_that("?config gives maxCells as the limit per call", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  local_mocked_bindings(.pxweb2_GET = v1_get())
  cfg <- pxweb2_get_config(v1_url)
  expect_identical(cfg$maxDataCells, 1000)
  expect_identical(cfg$maxCallsPerTimeWindow, 30L)
})

test_that("metadata gets the same shape as in API v2", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  local_mocked_bindings(.pxweb2_GET = v1_get())
  meta <- pxweb2_get_metadata("A/B%20C/T1.px", base_url = v1_url)
  expect_identical(meta$label, "Folkmängd")
  expect_identical(meta$extension$px$tableid, "A/B%20C/T1.px")

  v <- pxweb2_get_variables(meta)
  expect_identical(v$code, c("Region", "ContentsCode", "Tid"))
  expect_identical(v$role, c("geo", "contents", "time"))
  expect_identical(v$elimination, c(TRUE, FALSE, FALSE))
  expect_identical(unname(v$size), c(3L, 1L, 3L))

  expect_identical(pxr_get_codes(v1_url, "A/B%20C/T1.px", "Tid", starts_with = "202", n_chars = 4),
                   c("2022", "2023", "2024"))
  expect_identical(pxr_get_labels(v1_url, "A/B%20C/T1.px", "region", contains = "t"), c("Ett", "Två", "Tre"))
  expect_named(pxr_get_all_codes(v1_url, "A/B%20C/T1.px"), c("Region", "ContentsCode", "Tid"))
  expect_identical(pxr_get_all_labels(v1_url, "A/B%20C/T1.px")$Region, c("Ett", "Två", "Tre"))
  expect_equal(nrow(pxweb2_list_codelists(meta)), 0)
})

test_that("table ids are paths, and folders and missing tables give clear errors", {
  chk <- pxweb2rgui:::.pxweb2_check_table_id
  expect_invisible(chk("BE/BE0101/Min tabell", v1_url))
  expect_error(chk("https://x.example/api/v1/sv/db/T", v1_url), "is a URL")
  expect_error(chk("BE/BE0101"), "whitespace or '/'")

  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  local_mocked_bindings(.pxweb2_GET = v1_get())
  expect_error(pxweb2_get_metadata("finns/inte", base_url = v1_url), "was not found")
  expect_error(pxweb2_get_metadata("A", base_url = v1_url), "was not found|is a folder")
})

test_that("the table list is built by reading the folders, and kept for the session", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  calls <- new.env()
  local_mocked_bindings(.pxweb2_GET = v1_get(calls))
  expect_message(res <- pxr_get_all_tabs(v1_url), "reading every folder")
  expect_identical(res$tabellid, c("T0.px", "A/B%20C/T1.px"))
  expect_identical(res$tab_name, c("Tabell noll", "Tabell ett"))
  expect_equal(res$updated, as.Date(c("2026-01-02", "2026-06-01")))

  n <- length(calls$urls)
  expect_silent(again <- pxr_search_tabs(v1_url, search_str = "ETT"))
  expect_identical(again$tabellid, "A/B%20C/T1.px")
  expect_identical(length(calls$urls), n)

  expect_identical(pxr_get_all_tabs(v1_url, updated_since = "2026-03-01")$tabellid, "A/B%20C/T1.px")
})

test_that("the table list has the columns and folders the interface's tree needs", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  local_mocked_bindings(.pxweb2_GET = v1_get())
  cat <- suppressMessages(pxweb2rgui:::px_parse_catalogue(
    pxweb2_search_tables(base_url = v1_url, includeDiscontinued = "true")))
  expect_identical(cat$tables$id, c("T0.px", "A/B%20C/T1.px"))
  expect_false(any(cat$tables$discontinued))
  expect_true(all(is.na(cat$tables$first_period)))
  under <- cat$paths[cat$paths$table_id == "A/B%20C/T1.px", ]
  expect_identical(under$label, c("Ämne A", "Under"))
  expect_identical(under$key, c("A", "A/B%20C"))
  tree <- pxweb2rgui:::px_build_tree(cat$paths)
  expect_identical(tree$n, 2L)
})

test_that("a folder that cannot be read gives a warning, and the list is not kept", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  calls <- new.env()
  local_mocked_bindings(.pxweb2_GET = v1_get(calls, fail = "https://v1.example/api/v1/sv/db/A/"))
  expect_warning(res <- suppressMessages(pxr_get_all_tabs(v1_url)), "could not be read")
  expect_identical(res$tabellid, "T0.px")
  n <- length(calls$urls)
  suppressWarnings(suppressMessages(pxr_get_all_tabs(v1_url)))
  expect_gt(length(calls$urls), n)

  # den första mappen är ett fel, inte en tom lista
  pxweb2_clear_cache()
  local_mocked_bindings(.pxweb2_GET = v1_get(fail = "https://v1.example/api/v1/sv/db/"))
  expect_error(suppressMessages(pxr_get_all_tabs(v1_url)), "Could not read the table list")
})

test_that("sites with search list every table at once, with folder names", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  calls <- new.env()
  found <- '[{"id":"T1.px","path":"/A/B%20C","title":"Tabell ett","published":"2026-06-01T08:00:00"}]'
  local_mocked_bindings(.pxweb2_GET = v1_get(calls, search = found))
  expect_no_message(raw <- pxweb2_search_tables(base_url = v1_url))
  expect_identical(raw$id, "A/B%20C/T1.px")
  expect_identical(raw$updated, "2026-06-01T08:00:00")
  expect_identical(raw$paths[[1]][[1]]$label, c("Ämne A", "Under"))
  # bara mapparna ovanför en mapp behöver läsas
  expect_false(any(grepl("B%2520C/$", calls$urls)))
})

test_that("the request body has the form of API v1", {
  values <- list(Tid = data.frame(code = c("2022", "2023", "2024")))
  body <- list(selection = list(
    list(variableCode = "Region", valueCodes = list("*")),
    list(variableCode = "Kon", valueCodes = list("1", "2")),
    list(variableCode = "Alder", valueCodes = list("5")),
    list(variableCode = "Tid", valueCodes = list("top(2)"))
  ))
  res <- pxweb2rgui:::.pxweb1_post_body(body, values)
  json <- as.character(jsonlite::toJSON(res, auto_unbox = TRUE))
  expect_match(json, '{"code":"Region","selection":{"filter":"all","values":["*"]}}', fixed = TRUE)
  expect_match(json, '{"code":"Kon","selection":{"filter":"item","values":["1","2"]}}', fixed = TRUE)
  expect_match(json, '{"code":"Alder","selection":{"filter":"item","values":["5"]}}', fixed = TRUE)
  expect_match(json, '{"code":"Tid","selection":{"filter":"top","values":["2"]}}', fixed = TRUE)
  expect_match(json, '"response":{"format":"json-stat2"}', fixed = TRUE)

  body$selection[[4]]$valueCodes <- list("BOTTOM(2)")
  res <- pxweb2rgui:::.pxweb1_post_body(body, values)
  expect_identical(res$query[[4]]$selection, list(filter = "item", values = list("2022", "2023")))
})

test_that("data is fetched with POST to the table, and parsed in spite of a BOM", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  seen <- new.env()
  local_mocked_bindings(
    .pxweb2_GET = v1_get(),
    .pxweb2_POST = function(url, ..., body, query = NULL) {
      seen$url <- url; seen$body <- as.character(body); seen$query <- query
      v1_response(url, paste0("﻿", v1_data))
    }
  )
  d <- suppressWarnings(pxr_pxweb2_get_data(
    "A/T1.px",
    query = list(Region = "Ett", ContentsCode = "*", Tid = "9999"),
    base_url = "https://v1.example/api/v1/sv/db/", lang = "en", code_variables = "Region", quiet = TRUE
  ))
  expect_identical(seen$url, "https://v1.example/api/v1/en/db/A/T1.px")
  expect_null(seen$query)
  expect_match(seen$body, '{"code":"Region","selection":{"filter":"item","values":["01"]}}', fixed = TRUE)
  expect_match(seen$body, '{"code":"Tid","selection":{"filter":"item","values":["2024"]}}', fixed = TRUE)
  expect_identical(d$region_code, "01")
  expect_identical(d$value, 7)
})

# Webbplats där T finns i två mappar och U i en. Den svarar bara på hela sökvägen, som
# de webbplatser som inte hittar en tabell från enbart namnet. Med namn = TRUE hittar
# den också tabellen från namnet.
v1_dubbel_get <- function(calls = new.env(), lista = NULL, namn = FALSE) {
  function(url, ..., query = NULL) {
    calls$urls <- c(calls$urls, url)
    json <- if (grepl("?config", url, fixed = TRUE)) {
      '{"maxValues":100,"maxCells":1000,"maxCalls":30,"timeWindow":10}'
    } else if (grepl("?query=", url, fixed = TRUE)) {
      NULL
    } else if (grepl("/db/$", url)) {
      lista %||% '[{"id":"A","type":"l","text":"A"},{"id":"B","type":"l","text":"B"}]'
    } else if (grepl("/db/A/$", url)) {
      '[{"id":"T.px","type":"t","text":"Tabell T"},{"id":"U.px","type":"t","text":"Tabell U"}]'
    } else if (grepl("/db/B/$", url)) {
      '[{"id":"T.px","type":"t","text":"Tabell T"}]'
    } else if (grepl("/db/(A|B)/[TU]\\.px$", url) || (namn && grepl("/db/[TU]$", url))) {
      v1_table
    }
    if (is.null(json)) return(v1_response(url, "{}", 400L))
    v1_response(url, json)
  }
}

# Koderna för år är löpnummer. En tabell med bara året, en med region också.
v1_lopnr <- function(seen) {
  list(
    get = function(url, ..., query = NULL) {
      if (grepl("?config", url, fixed = TRUE)) {
        return(v1_response(url, '{"maxValues":100,"maxCells":1000,"maxCalls":30,"timeWindow":10}'))
      }
      if (grepl("/En\\.px$", url)) {
        return(v1_response(url, '{"title":"En","variables":[
          {"code":"År","text":"År","values":["0","1","2"],"valueTexts":["2023","2024","2025"],"time":true}]}'))
      }
      if (grepl("/Tva\\.px$", url)) {
        return(v1_response(url, '{"title":"Två","variables":[
          {"code":"Kön","text":"Kön","values":["0","1"],"valueTexts":["Man","Kvinna"]},
          {"code":"Läsår","text":"Läsår","values":["0","1"],"valueTexts":["2024/2025","2025/2026"]}]}'))
      }
      v1_response(url, "{}", 400L)
    },
    post = function(url, ..., body) {
      seen$body <- as.character(body)
      v1_response(url, '{"version":"2.0","class":"dataset","id":["År"],"size":[1],
        "dimension":{"År":{"label":"År","category":{"index":{"2":0},"label":{"2":"2025"}}}},"value":[7]}')
    }
  )
}

test_that("labels, 9999 and from(), to(), range() work when codes are running numbers", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  seen <- new.env()
  m <- v1_lopnr(seen)
  local_mocked_bindings(.pxweb2_GET = m$get, .pxweb2_POST = m$post)
  skickat <- function(v) {
    q <- list(v); names(q) <- "År"
    suppressWarnings(pxr_pxweb2_get_data("En.px", query = q, base_url = v1_url, quiet = TRUE))
    as.character(jsonlite::fromJSON(seen$body)$query$selection$values[[1]])
  }
  expect_identical(skickat("2025"), "2")
  expect_identical(skickat(c("2023", "2025")), c("0", "2"))
  expect_identical(skickat("9999"), "2")
  expect_identical(skickat("from(2024)"), c("1", "2"))
  expect_identical(skickat("to(2024)"), c("0", "1"))
  expect_identical(skickat("range(2023, 2024)"), c("0", "1"))
  # ett värde som är en kod betyder koden
  expect_identical(skickat("1"), "1")
  expect_identical(skickat("from(1)"), c("1", "2"))
  # ett värde som saknas stoppas här, med tabellens värden, och skyller inte på servern
  seen$body <- NULL
  expect_error(skickat("1999"), "Invalid values.*The table has these values: 2023, 2024, 2025")
  expect_null(seen$body)
})

test_that("a table with several variables and running numbers works the same way", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  seen <- new.env()
  m <- v1_lopnr(seen)
  local_mocked_bindings(.pxweb2_GET = m$get, .pxweb2_POST = m$post)
  q <- list("Kvinna", "9999"); names(q) <- c("Kön", "Läsår")
  suppressWarnings(pxr_pxweb2_get_data("Tva.px", query = q, base_url = v1_url, quiet = TRUE))
  sel <- jsonlite::fromJSON(seen$body)$query$selection$values
  expect_identical(as.character(sel[[1]]), "1")
  # Läsår är inte markerad som tid på webbplatsen, men känns igen på namnet
  expect_identical(as.character(sel[[2]]), "1")
})

test_that("an id that matches tables in several folders is an error that lists them", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  local_mocked_bindings(
    .pxweb2_GET = v1_dubbel_get(),
    .pxweb2_POST = function(url, ...) stop("no data should be fetched")
  )
  q <- list(Region = "01", ContentsCode = "*", Tid = "2024")
  flertydig <- "matches several tables.*\"A/T\\.px\", \"B/T\\.px\""
  suppressMessages(expect_error(pxr_get_all_labels(v1_url, "T"), flertydig))
  expect_error(pxr_get_all_labels(v1_url, "T.px"), flertydig)
  expect_error(pxr_get_all_labels(v1_url, "t"), flertydig)
  expect_error(pxr_get_all_codes(v1_url, "T"), flertydig)
  expect_error(pxr_get_labels(v1_url, "T", "Region"), flertydig)
  expect_error(pxr_get_codes(v1_url, "T", "Region"), flertydig)
  expect_error(pxweb2_get_metadata("T", base_url = v1_url), flertydig)
  expect_error(pxweb2_table_updated("T", base_url = v1_url), flertydig)
  expect_error(pxr_pxweb2_get_data("T", query = q, base_url = v1_url, quiet = TRUE), flertydig)
  # flera tabeller, där en av dem är flertydig
  expect_error(pxr_get_all_labels(v1_url, c("U", "T")), flertydig)
  expect_error(pxr_pxweb2_get_data(c("A/U.px", "T"), query = q, base_url = v1_url, quiet = TRUE), flertydig)
})

test_that("an unambiguous id is turned into the path, and a full path works as before", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  calls <- new.env()
  seen <- new.env()
  local_mocked_bindings(
    .pxweb2_GET = v1_dubbel_get(calls),
    .pxweb2_POST = function(url, ...) {
      seen$url <- url
      v1_response(url, v1_data)
    }
  )
  # en fullständig sökväg behöver ingen tabellista
  expect_identical(pxr_get_all_labels(v1_url, "A/T.px")$Region, c("Ett", "Två", "Tre"))
  expect_identical(pxr_get_all_labels(v1_url, "B/T.px")$Region, c("Ett", "Två", "Tre"))
  expect_false(any(grepl("/db/$", calls$urls)))
  # ett namn som bara finns i en mapp, med och utan .px
  expect_identical(suppressMessages(pxr_get_all_labels(v1_url, "U"))$Region, c("Ett", "Två", "Tre"))
  expect_identical(pxr_get_all_labels(v1_url, "U.px")$Region, c("Ett", "Två", "Tre"))
  expect_identical(pxr_get_all_labels(v1_url, "u")$Region, c("Ett", "Två", "Tre"))
  expect_identical(pxweb2_get_metadata("U", base_url = v1_url)$extension$px$tableid, "A/U.px")
  d <- pxr_pxweb2_get_data("U", query = list(Region = "01", ContentsCode = "*", Tid = "2024"),
                           base_url = v1_url, quiet = TRUE)
  expect_identical(d$value, 7)
  expect_identical(seen$url, "https://v1.example/api/v1/sv/db/A/U.px")
})

test_that("a repeated path or a table at the top level is not ambiguous", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  resolve <- pxweb2rgui:::.pxweb1_resolve_id
  local_mocked_bindings(.pxweb1_all_tables = function(...) {
    tibble::tibble(id = c("A/T.px", "A/T.px", "U.px", "A/U.px"))
  })
  expect_identical(resolve("T", v1_url, "sv"), "A/T.px")
  # U.px är tabellen högst upp, med den sökvägen
  expect_identical(resolve("U.px", v1_url, "sv"), "U.px")
  expect_error(resolve("U", v1_url, "sv"), "matches several tables")
  # ett namn som inte finns, och en sökväg, lämnas åt webbplatsen
  expect_identical(resolve("X", v1_url, "sv"), "X")
  expect_identical(resolve("Z/T.px", v1_url, "sv"), "Z/T.px")
})

test_that("a table list that cannot be read leaves an id without folders to the site", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  local_mocked_bindings(.pxweb1_all_tables = function(...) stop("no list"),
                        .pxweb2_GET = v1_dubbel_get(namn = TRUE))
  expect_identical(pxr_get_all_labels(v1_url, "T")$Region, c("Ett", "Två", "Tre"))
})

test_that("a table the server cannot deliver as JSON-stat gives a clear error", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  local_mocked_bindings(
    .pxweb2_GET = v1_get(),
    .pxweb2_POST = function(url, ...) v1_response(url, "<html>404 - File or directory not found.</html>", 404L)
  )
  q <- list(Region = "01", ContentsCode = "*", Tid = "2024")
  # serverns html-sida ska inte skrivas ut
  expect_output(
    expect_error(pxr_pxweb2_get_data("A/B%20C/T1.px", query = q, base_url = v1_url, quiet = TRUE),
                 "exists on .* could not deliver .*JSON-stat .*HTTP 404"),
    NA
  )
})

test_that("other HTTP errors on data in API v1 are not reported as a JSON-stat problem", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  local_mocked_bindings(
    .pxweb2_GET = v1_get(),
    .pxweb2_POST = function(url, ...) v1_response(url, "fel", 403L)
  )
  q <- list(Region = "01", ContentsCode = "*", Tid = "2024")
  capture.output(err <- tryCatch(pxr_pxweb2_get_data("A/B%20C/T1.px", query = q, base_url = v1_url, quiet = TRUE),
                                 error = conditionMessage))
  expect_match(err, "403")
  expect_no_match(err, "JSON-stat")
})

test_that("include_aggregations gives a warning in API v1, and codelists an error", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  local_mocked_bindings(
    .pxweb2_GET = v1_get(),
    .pxweb2_POST = function(url, ...) v1_response(url, v1_data)
  )
  q <- list(Region = "01", ContentsCode = "*", Tid = "2024")
  expect_warning(pxr_pxweb2_get_data("T1.px", query = q, base_url = v1_url, include_aggregations = "all", quiet = TRUE),
                 "no code lists")
  expect_no_warning(pxr_pxweb2_get_data("T1.px", query = q, base_url = v1_url, quiet = TRUE))
  expect_error(pxweb2_get_codelist("vs_Region", base_url = v1_url), "no code lists")
})

test_that("an eliminated variable is not counted and needs no code column", {
  # gäller båda API-versionerna: en eliminerad variabel finns varken i frågan eller svaret
  variables <- data.frame(code = c("Region", "Tid"), label = c("region", "år"), size = c(300L, 5L),
                          elimination = c(TRUE, FALSE), role = c("geo", "time"), show = NA_character_)
  q <- list(selection = list(list(variableCode = "Tid", valueCodes = list("2024"))))
  expect_identical(pxweb2rgui:::.pxweb2_count_cells(variables, q), 1)

  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  no_region <- '{"version":"2.0","class":"dataset","id":["ContentsCode","Tid"],"size":[1,1],
    "dimension":{
    "ContentsCode":{"label":"tabellinnehåll","category":{"index":{"N1":0},"label":{"N1":"Antal"}}},
    "Tid":{"label":"år","category":{"index":{"2024":0},"label":{"2024":"2024"}}}},
    "value":[7]}'
  local_mocked_bindings(
    .pxweb2_GET = v1_get(),
    .pxweb2_POST = function(url, ...) v1_response(url, no_region)
  )
  d <- pxr_pxweb2_get_data("T1.px", query = list(Region = NA, ContentsCode = "*", Tid = "2024"),
                           base_url = v1_url, code_variables = "*", quiet = TRUE)
  expect_named(d, c("tabellinnehåll_code", "tabellinnehåll", "år_code", "år", "value"))
})

test_that("a time variable that the site does not mark is found by its name", {
  rolle <- function(...) {
    vars <- lapply(list(...), function(x) list(code = x[[1]], text = x[[1]], values = list("1"),
                                               time = if (length(x) > 1) x[[2]]))
    unlist(.pxweb1_metadata(list(title = "T", variables = vars), "T")$role$time)
  }
  expect_identical(rolle(list("region"), list("tid"), list("Personalgrupp")), "tid")
  expect_identical(rolle(list("År"), list("kön")), "År")
  # bara den första, och inget om inget namn stämmer
  expect_identical(rolle(list("år"), list("månad")), "år")
  expect_null(rolle(list("region"), list("Personalgrupp")))
  # en markering från webbplatsen går före namnet
  expect_identical(rolle(list("tid"), list("Omgång", TRUE)), "Omgång")
})

test_that("a code column leaves numbers in other labels than regions alone", {
  # med koden 20 ska "20 år" inte bli "år", medan regionens kod tas bort
  dim <- function(label, codes, labels, ...) {
    list(label = label, extension = list(...),
         category = list(index = as.list(stats::setNames(seq_along(codes) - 1L, codes)),
                         label = as.list(stats::setNames(labels, codes))))
  }
  meta <- list(
    label = "T", id = list("Region", "Alder", "Grupp"),
    role = list(geo = list("Region")),
    dimension = list(Region = dim("region", "0114", "0114 Upplands Väsby"),
                     Alder = dim("ålder", "20", "20 år"),
                     Grupp = dim("grupp", "7", "7 Sju", show = "code_value")),
    extension = list(px = list(tableid = "T"))
  )
  json <- '{"version":"2.0","class":"dataset","id":["Region","Alder","Grupp"],"size":[1,1,1],
    "dimension":{
    "Region":{"label":"region","category":{"index":{"0114":0},"label":{"0114":"0114 Upplands Väsby"}}},
    "Alder":{"label":"ålder","category":{"index":{"20":0},"label":{"20":"20 år"}}},
    "Grupp":{"label":"grupp","category":{"index":{"7":0},"label":{"7":"7 Sju"}}}},
    "value":[5]}'
  local_mocked_bindings(
    .pxweb2_get_config = function(...) list(maxDataCells = 1000, maxCallsPerTimeWindow = 30, timeWindow = 10),
    .pxweb2_POST = function(url, ...) v1_response(url, json)
  )
  d <- pxr_pxweb2_get_data(meta, query = list(Region = "*", Alder = "*", Grupp = "*"), lang = "sv",
                           base_url = "https://statistikdatabasen.scb.se/api/v2/tables/",
                           code_variables = c("Region", "Alder"), quiet = TRUE,
                           split_deso_regso_by_municipality = FALSE)
  expect_identical(d[["ålder"]], "20 år")
  expect_identical(d[["ålder_code"]], "20")
  expect_identical(d[["region"]], "Upplands Väsby")
  expect_identical(d[["grupp"]], "Sju")
  expect_false("grupp_code" %in% names(d))
})

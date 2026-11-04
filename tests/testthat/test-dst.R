# Danmarks Statistik, utan nätverk

dst_response <- function(url, json, status = 200L) {
  structure(list(url = url, status_code = status,
                 headers = structure(list(`content-type` = "text/json; charset=utf-8"),
                                     class = c("insensitive", "list")),
                 content = charToRaw(enc2utf8(json))),
            class = "response")
}

dst_info <- '{"id":"FOLK1A","text":"Folketal","unit":"Antal","updated":"2026-08-10T08:00:00","active":true,
  "variables":[
  {"id":"OMRÅDE","text":"område","elimination":true,"time":false,"map":"denmark_municipality_07",
   "values":[{"id":"000","text":"Hele landet"},{"id":"101","text":"København"}]},
  {"id":"KØN","text":"køn","elimination":true,"time":false,
   "values":[{"id":"1","text":"Mænd"},{"id":"2","text":"Kvinder"}]},
  {"id":"Tid","text":"tid","elimination":false,"time":true,
   "values":[{"id":"2026K1","text":"2026K1"},{"id":"2026K2","text":"2026K2"},{"id":"2026K3","text":"2026K3"}]}]}'

dst_tables <- '[
  {"id":"FOLK1A","text":"Folketal","updated":"2026-08-10T08:00:00","firstPeriod":"2008K1","latestPeriod":"2026K3","active":true,"variables":["område"]},
  {"id":"GAMMEL","text":"Gammel tabel","updated":"2001-01-01T08:00:00","firstPeriod":"1990","latestPeriod":"2000","active":false,"variables":["tid"]},
  {"id":"LOES","text":"Tabel uden emne","updated":"2026-01-01T08:00:00","firstPeriod":"2020","latestPeriod":"2025","active":true,"variables":["tid"]}]'

dst_subjects <- '[{"id":"1","description":"Borgere","subjects":[
  {"id":"3401","description":"Befolkning\\n","subjects":[],"tables":[{"id":"FOLK1A"}]},
  {"id":"3402","description":"Andet","subjects":[],"tables":[{"id":"FOLK1A"},{"id":"GAMMEL"}]}],
  "tables":[]}]'

# JSON-stat 1, som hos Danmarks Statistik
dst_data <- '{"dataset":{"dimension":{
  "OMRÅDE":{"label":"område","category":{"index":{"101":0},"label":{"101":"København"}}},
  "ContentsCode":{"label":"Indhold","category":{"index":{"FOLK1A":0},"label":{"FOLK1A":"Folketal"}}},
  "Tid":{"label":"tid","category":{"index":{"2026K2":0,"2026K3":1},"label":{"2026K2":"2026K2","2026K3":"2026K3"}}},
  "id":["OMRÅDE","ContentsCode","Tid"],"size":[1,1,2],"role":{"metric":["ContentsCode"],"time":["Tid"]}},
  "label":"Folketal","source":"Danmarks Statistik","updated":"2026-08-10T06:00:00Z","value":[670389,671000]}}'

dst_get <- function(calls = new.env()) {
  function(url, ..., query = NULL) {
    calls$urls <- c(calls$urls, url); calls$query <- c(calls$query, list(query))
    if (grepl("tableinfo/FOLK1A", url, fixed = TRUE)) return(dst_response(url, dst_info))
    if (grepl("tableinfo/", url, fixed = TRUE)) return(dst_response(url, '{"errorTypeCode":"EXTRACT-NOTFOUND"}', 400L))
    if (endsWith(url, "/tables")) return(dst_response(url, dst_tables))
    if (endsWith(url, "/subjects")) return(dst_response(url, dst_subjects))
    dst_response(url, "{}", 404L)
  }
}

test_that("the address is Statistics Denmark and not PxWeb API v1", {
  f <- pxweb2rgui:::.pxweb2_normalise_base_url
  url <- "https://api.statbank.dk/v1/"
  expect_identical(pxweb2_base_url("dst"), url)
  expect_identical(f("https://api.statbank.dk/v1"), url)
  expect_identical(f("https://api.statbank.dk"), url)
  expect_false(pxweb2rgui:::.pxweb1_is_url(url))
  expect_identical(pxweb2rgui:::px_tables_url("https://api.statbank.dk/v1/tables"), url)
})

test_that("languages are da and en, and others are stopped before any call", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  calls <- new.env()
  local_mocked_bindings(.pxweb2_GET = dst_get(calls))
  l <- pxweb2_languages("dst")
  expect_identical(l$id, c("da", "en"))
  expect_identical(attr(l, "default"), "da")
  expect_error(pxr_get_all_codes("dst", "FOLK1A", lang = "sv"), "not offered")
  expect_null(calls$urls)
})

test_that("metadata gets the same shape as in API v2", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  calls <- new.env()
  local_mocked_bindings(.pxweb2_GET = dst_get(calls))
  meta <- pxweb2_get_metadata("FOLK1A", base_url = "dst", lang = "en")
  expect_identical(calls$query[[1]]$lang, "en")
  expect_identical(meta$label, "Folketal")
  expect_identical(meta$updated, "2026-08-10T08:00:00Z")
  v <- pxweb2_get_variables(meta)
  expect_identical(v$code, c("OMRÅDE", "KØN", "Tid"))
  expect_identical(v$role, c("geo", "other", "time"))
  expect_identical(v$elimination, c(TRUE, TRUE, FALSE))

  expect_identical(pxr_get_codes("dst", "FOLK1A", "køn", lang = "en"), c("1", "2"))
  expect_identical(pxr_get_labels("dst", "FOLK1A", "OMRÅDE", contains = "køben"), "København")
  expect_named(pxr_get_all_labels("dst", "FOLK1A"), c("OMRÅDE", "KØN", "Tid"))
  expect_equal(nrow(pxweb2_list_codelists(meta)), 0)
  expect_error(pxweb2_get_metadata("FINNSINTE", base_url = "dst"), "was not found")
  expect_true(pxweb2_table_exists("FOLK1A", "dst"))
  expect_false(pxweb2_table_exists("FINNSINTE", "dst"))
})

test_that("the table list has dates, periods, active flag and folders, and is cached", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  calls <- new.env()
  local_mocked_bindings(.pxweb2_GET = dst_get(calls))
  res <- pxr_get_all_tabs("dst")
  expect_identical(res$tabellid, c("FOLK1A", "LOES"))
  expect_equal(res$updated, as.Date(c("2026-08-10", "2026-01-01")))
  n <- length(calls$urls)
  expect_identical(pxr_search_tabs("dst", search_str = "UDEN")$tabellid, "LOES")
  expect_identical(pxr_get_all_tabs("dst", updated_since = "2026-05-01")$tabellid, "FOLK1A")
  expect_identical(length(calls$urls), n)

  # gränssnittets träd: avslutade tabeller med, och en tabell kan ligga i två mappar
  cat <- pxweb2rgui:::px_parse_catalogue(pxweb2_search_tables(base_url = "dst", includeDiscontinued = "true"))
  expect_identical(cat$tables$discontinued, c(FALSE, TRUE, FALSE))
  expect_identical(cat$tables$first_period[1], "2008K1")
  expect_identical(cat$tables$subject_code[1], "1")
  folk <- cat$paths[cat$paths$table_id == "FOLK1A", ]
  expect_setequal(folk$key, c("1", "1/3401", "1/3402"))
  expect_identical(folk$label[folk$key == "1/3401"], "Befolkning")
  expect_identical(cat$paths$label[cat$paths$table_id == "LOES"], "(Utan ämnesområde)")
})

test_that("the request body uses codes, since (2) means the second latest period", {
  values <- list(Tid = data.frame(code = c("2026K1", "2026K2", "2026K3")))
  body <- list(selection = list(list(variableCode = "Tid", valueCodes = list("top(2)"))))
  f <- pxweb2rgui:::.dst_post_body
  res <- f(body, "FOLK1A", "en", values)
  expect_identical(res$table, "FOLK1A")
  expect_identical(res$format, "JSONSTAT")
  expect_identical(res$lang, "en")
  expect_identical(res$variables[[1]], list(code = "Tid", values = list("2026K2", "2026K3")))
  body$selection[[1]]$valueCodes <- list("BOTTOM(1)")
  expect_identical(f(body, "FOLK1A", "en", values)$variables[[1]]$values, list("2026K1"))
  body$selection[[1]]$valueCodes <- list("*")
  expect_identical(f(body, "FOLK1A", "en", values)$variables[[1]]$values, list("*"))
  json <- as.character(jsonlite::toJSON(res, auto_unbox = TRUE))
  expect_match(json, '"values":["2026K2","2026K3"]', fixed = TRUE)
})

test_that("JSON-stat 1 is parsed by the same parser", {
  p <- pxweb2rgui:::.pxweb2_parse_jsonstat2(dst_data, code_dims = "OMRÅDE")
  expect_named(p$labels, c("område", "Indhold", "tid", "value"))
  expect_identical(p$labels$value, c(670389, 671000))
  expect_identical(p$codes$`OMRÅDE`, c("101", "101"))
})

test_that("data is fetched with POST, and aggregations give a warning", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  seen <- new.env()
  local_mocked_bindings(
    .pxweb2_GET = dst_get(),
    .pxweb2_POST = function(url, ..., body, query = NULL) {
      seen$url <- url; seen$body <- as.character(body); seen$query <- query
      dst_response(url, dst_data)
    }
  )
  q <- list(`OMRÅDE` = "København", `KØN` = NA, Tid = "top(2)")
  d <- pxr_pxweb2_get_data("FOLK1A", query = q, base_url = "dst", code_variables = "OMRÅDE", quiet = TRUE)
  expect_identical(seen$url, "https://api.statbank.dk/v1/data")
  expect_null(seen$query)
  expect_match(seen$body, '{"code":"OMRÅDE","values":["101"]}', fixed = TRUE)
  expect_false(grepl("KØN", seen$body, fixed = TRUE))
  expect_identical(d$value, c(670389, 671000))
  expect_identical(d$`område_kod`, c("101", "101"))
  expect_warning(pxr_pxweb2_get_data("FOLK1A", query = q, base_url = "dst", include_aggregations = "all", quiet = TRUE),
                 "Statistics Denmark's API has no code lists")
  expect_error(pxweb2_get_codelist("x", base_url = "dst"), "no code lists")
})

test_that("a large request is split below the limit, counted as rows x columns", {
  variables <- data.frame(code = c("A", "B", "C", "D", "E"))
  expect_identical(pxweb2rgui:::.dst_max_rows(variables), floor(1e6 / 7))
})

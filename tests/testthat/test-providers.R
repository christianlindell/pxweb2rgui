fake_json_response <- function(url, json, status = 200L) {
  structure(list(url = url, status_code = status,
                 headers = structure(list(`content-type` = "application/json; charset=utf-8"),
                                     class = c("insensitive", "list")),
                 content = charToRaw(json)),
            class = "response")
}

test_that("providers include SCB and SSB and resolve to the tables endpoint", {
  p <- pxweb2_providers()
  expect_true(all(c("scb", "ssb") %in% p$id))
  expect_identical(pxweb2_base_url("scb"), "https://statistikdatabasen.scb.se/api/v2/tables/")
  expect_identical(pxweb2_base_url("SSB"), "https://data.ssb.no/api/pxwebapi/v2/tables/")
  expect_error(pxweb2_base_url("finns_inte"), "Unknown provider")
})

test_that("base_url is normalised whether given as root or tables endpoint", {
  f <- pxweb2rgui:::.pxweb2_normalise_base_url
  expect_identical(f("https://example.org/api/v2"), "https://example.org/api/v2/tables/")
  expect_identical(f("https://example.org/api/v2/"), "https://example.org/api/v2/tables/")
  expect_identical(f("https://example.org/api/v2/tables"), "https://example.org/api/v2/tables/")
  expect_identical(f("https://example.org/api/v2/tables/"), "https://example.org/api/v2/tables/")
  expect_identical(pxweb2rgui:::.pxweb2_api_root("ssb"), "https://data.ssb.no/api/pxwebapi/v2/")
})

test_that("custom providers can be added and the default changed with options", {
  old <- options(pxweb2r.providers = NULL, pxweb2r.base_url = NULL)
  on.exit(options(old))
  pxweb2_add_provider("test", "https://example.org/api/v2", name = "Testbyrån",
                      default_lang = "fi", languages = c("fi", "sv"))
  p <- pxweb2_providers()
  expect_identical(p$tables_url[p$id == "test"], "https://example.org/api/v2/tables/")
  expect_identical(pxweb2_base_url("test"), "https://example.org/api/v2/tables/")
  options(pxweb2r.base_url = "test")
  expect_identical(pxweb2rgui:::.pxweb2_normalise_base_url(NULL), "https://example.org/api/v2/tables/")
})

test_that("languages are read from /config and lang is validated", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  local_mocked_bindings(
    .pxweb2_GET = function(url, ...) fake_json_response(url,
      '{"languages":[{"id":"no","label":"Norsk"},{"id":"en","label":"English"}],
        "defaultLanguage":"no","maxDataCells":800000,"maxCallsPerTimeWindow":30,"timeWindow":60}'),
    .package = "pxweb2rgui"
  )
  l <- pxweb2_languages("ssb")
  expect_identical(l$id, c("no", "en"))
  expect_identical(attr(l, "default"), "no")
  b <- pxweb2_base_url("ssb")
  expect_identical(pxweb2rgui:::.pxweb2_resolve_lang(NULL, b), "no")
  expect_identical(pxweb2rgui:::.pxweb2_resolve_lang("en", b), "en")
  expect_error(pxweb2rgui:::.pxweb2_resolve_lang("sv", b), "not offered")
})

test_that("without /config the provider list gives the languages", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  local_mocked_bindings(
    .pxweb2_GET = function(url, ...) structure(list(url = url, status_code = 404L), class = "response"),
    .package = "pxweb2rgui"
  )
  l <- pxweb2_languages("scb")
  expect_identical(l$id, c("sv", "en"))
  expect_identical(pxweb2rgui:::.pxweb2_resolve_lang(NULL, pxweb2_base_url("scb")), "sv")
  # an unknown site without /config: default "sv", any language accepted
  expect_identical(pxweb2rgui:::.pxweb2_resolve_lang(NULL, "https://example.org/api/v2/tables/"), "sv")
  expect_identical(pxweb2rgui:::.pxweb2_resolve_lang("fi", "https://example.org/api/v2/tables/"), "fi")
})

test_that("metadata is fetched with lang and cached per language", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  calls <- list()
  local_mocked_bindings(
    .pxweb2_GET = function(url, ..., query = NULL) {
      if (grepl("/config$", url)) return(structure(list(url = url, status_code = 404L), class = "response"))
      calls[[length(calls) + 1]] <<- list(url = url, lang = query$lang)
      fake_json_response(url, paste0('{"label":"', query$lang, '"}'))
    },
    .package = "pxweb2rgui"
  )
  sv <- pxweb2_get_metadata("TAB1", base_url = "scb", lang = "sv")
  en <- pxweb2_get_metadata("TAB1", base_url = "scb", lang = "en")
  sv2 <- pxweb2_get_metadata("TAB1", base_url = "scb", lang = "sv")
  expect_identical(sv$label, "sv")
  expect_identical(en$label, "en")
  expect_identical(sv2$label, "sv")
  expect_length(calls, 2)
  expect_identical(calls[[1]]$url, "https://statistikdatabasen.scb.se/api/v2/tables/TAB1/metadata")
  expect_identical(vapply(calls, `[[`, "", "lang"), c("sv", "en"))
})

test_that("search_tables uses the provider's tables endpoint and language", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  seen <- NULL
  local_mocked_bindings(
    .pxweb2_GET = function(url, ..., query = NULL) {
      if (grepl("/config$", url)) return(structure(list(url = url, status_code = 404L), class = "response"))
      seen <<- list(url = url, lang = query$lang)
      fake_json_response(url, '{"tables":[{"id":"07459","label":"Befolkning"}]}')
    },
    .package = "pxweb2rgui"
  )
  res <- pxweb2_search_tables(base_url = "ssb", lang = "en")
  expect_identical(seen$url, "https://data.ssb.no/api/pxwebapi/v2/tables")
  expect_identical(seen$lang, "en")
  expect_identical(res$id, "07459")
})

test_that("pxweb2_add_provider without languages does not invent the language 'NA'", {
  old <- options(pxweb2r.providers = NULL)
  on.exit(options(old), add = TRUE)
  pxweb2_add_provider("testprov", "https://example.invalid/api/v2/")
  p <- pxweb2_providers()
  expect_true(is.na(p$languages[p$id == "testprov"]))
  cfg <- list(maxDataCells = 150000)
  langs <- pxweb2rgui:::.pxweb2_languages_from_config(cfg, "https://example.invalid/api/v2/tables/")
  expect_equal(nrow(langs), 0)
})

test_that(".pxweb2_resolve_lang returns the API's spelling of the language", {
  testthat::local_mocked_bindings(
    .pxweb2_get_config = function(base_url) list(
      languages = list(list(id = "sv", label = "Svenska"), list(id = "en", label = "English")),
      defaultLanguage = "sv"),
    .package = "pxweb2rgui")
  expect_equal(pxweb2rgui:::.pxweb2_resolve_lang("EN", "https://x.invalid/api/v2/tables/"), "en")
  expect_error(pxweb2rgui:::.pxweb2_resolve_lang("fi", "https://x.invalid/api/v2/tables/"), "not offered")
})

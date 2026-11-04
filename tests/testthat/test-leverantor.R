test_that("koden anger alltid språk och webbplats", {
  query <- list(Region = "00", Tid = "9999")
  code <- px_query_code("07459", "Befolkning", query, lang = "no",
                        base_url = "https://data.ssb.no/api/pxwebapi/v2/tables/",
                        provider_name = "Statistisk sentralbyrå (SSB), Norge")
  expect_match(code, 'lang = "no"', fixed = TRUE)
  expect_match(code, 'base_url = "https://data.ssb.no/api/pxwebapi/v2/tables/"', fixed = TRUE)
  expect_match(code, "# Statistisk sentralbyrå (SSB), Norge", fixed = TRUE)
  call <- parse(text = code)[[2]][[3]]
  expect_identical(call$lang, "no")
  expect_identical(call$base_url, "https://data.ssb.no/api/pxwebapi/v2/tables/")
  # standard är SCB på svenska
  code <- px_query_code("TAB1", "t", query)
  expect_match(code, 'lang = "sv"', fixed = TRUE)
  expect_match(code, 'base_url = "https://statistikdatabasen.scb.se/api/v2/tables/"', fixed = TRUE)
})

test_that("kommentaren i koden anger namnet att använda som provider", {
  query <- list(Region = "00", Tid = "9999")
  rad <- function(...) strsplit(px_query_code("TAB1", "Titel", query, ...), "\n")[[1]][3:4]
  expect_identical(rad(provider_name = "Statistiska centralbyrån (SCB), Sverige", provider = "scb"),
                   c('# Statistiska centralbyrån (SCB), Sverige (provider = "scb")', "# Titel"))
  # en egen webbplats anges med sin adress
  expect_identical(rad(provider_name = "x.org", provider = "https://x.org/api/v2/tables/")[1],
                   '# x.org (provider = "https://x.org/api/v2/tables/")')
  # utan provider är kommentaren bara leverantörens namn
  expect_identical(rad(provider_name = "x.org")[1], "# x.org")
  # kommentaren ändrar inte koden, som fortfarande går att tolka
  code <- px_query_code("TAB1", "Titel", query, provider_name = "x.org", provider = "scb")
  expect_identical(length(parse(text = code)), 2L)
})

test_that("adresser blir tabellernas endpoint", {
  expect_identical(px_tables_url("https://x.org/api/v2"), "https://x.org/api/v2/tables/")
  expect_identical(px_tables_url("https://x.org/api/v2/tables"), "https://x.org/api/v2/tables/")
  expect_identical(px_tables_url("https://x.org/api/v2/tables/"), "https://x.org/api/v2/tables/")
})

test_that("en cachefil per webbplats och språk", {
  old <- options(pxweb2rgui.cache_file = NULL)
  on.exit(options(old))
  a <- px_cache_file("https://statistikdatabasen.scb.se/api/v2/tables/", "sv")
  b <- px_cache_file("https://statistikdatabasen.scb.se/api/v2/tables/", "en")
  c <- px_cache_file("https://data.ssb.no/api/pxwebapi/v2/tables/", "en")
  expect_false(a == b || b == c || a == c)
  expect_match(basename(a), "statistikdatabasen.scb.se_api_v2_sv.rds", fixed = TRUE)
  options(pxweb2rgui.cache_file = "/tmp/x.rds")
  expect_identical(px_cache_file("https://data.ssb.no/api/pxwebapi/v2/tables/", "no"),
                   "/tmp/x_data.ssb.no_api_pxwebapi_v2_no.rds")
})

test_that("argument som pxweb2r saknar tas bort", {
  env <- new.env()
  env$f <- function(table_id, base_url = "x") list(table_id = table_id, base_url = base_url)
  local_mocked_bindings(px_fun = function(name) env$f)
  expect_identical(px_call_pxweb2r("f", "T1", base_url = "u", lang = "en"),
                   list(table_id = "T1", base_url = "u"))
})

test_that("språklistan visar svenska namn", {
  ch <- px_language_choices(data.frame(id = c("no", "en", "xx"), label = c("Norsk", "English", "Xish")))
  expect_identical(unname(ch), c("no", "en", "xx"))
  expect_identical(names(ch), c("norska (no)", "engelska (en)", "Xish (xx)"))
})

test_that("byte av leverantör och språk ger ny tabellista och rätt anrop", {
  calls <- list()
  b <- mock_backend()
  st <- b$search_tables; gm <- b$get_metadata; gd <- b$get_data
  b$search_tables <- function(query = NULL, ..., base_url = NULL, lang = NULL) {
    calls[[length(calls) + 1]] <<- list(base_url = base_url, lang = lang)
    st(query)
  }
  meta_args <- NULL
  b$get_metadata <- function(table_id, ...) { meta_args <<- list(...); gm(table_id) }
  data_args <- NULL
  b$get_data <- function(...) { data_args <<- list(...); gd(...) }
  old <- options(pxweb2rgui.backend = b, pxweb2rgui.cache_file = tempfile(fileext = ".rds"))
  on.exit(options(old))

  testServer(px_app_server(), {
    session$setInputs(leverantor = "scb", sprak = "sv")
    expect_identical(site()$base_url, "https://scb.example/api/v2/tables/")
    expect_identical(calls[[length(calls)]], list(base_url = "https://scb.example/api/v2/tables/", lang = "sv"))
    expect_identical(max_celler(), 150000)

    # SSB: språklistan byts, "sv" finns inte där så norska väljs
    session$setInputs(leverantor = "ssb")
    expect_identical(sprak_koder(), c("no", "en"))
    session$setInputs(sprak = "en")
    expect_identical(site()$lang, "en")
    expect_identical(calls[[length(calls)]], list(base_url = "https://ssb.example/api/v2/tables/", lang = "en"))
    expect_identical(max_celler(), 800000)

    session$setInputs(vald_tabell = "TAB6104")
    expect_identical(meta_args, list(base_url = "https://ssb.example/api/v2/tables/", lang = "en"))
    expect_match(fraga()$code, 'lang = "en"', fixed = TRUE)
    expect_match(fraga()$code, 'base_url = "https://ssb.example/api/v2/tables/"', fixed = TRUE)
    session$setInputs(hamta_data = 1)
    expect_identical(data_args$base_url, "https://ssb.example/api/v2/tables/")
    expect_identical(data_args$lang, "en")

    # byte av språk nollställer vald tabell
    session$setInputs(sprak = "no")
    expect_null(tabell())

    # egen adress
    session$setInputs(leverantor = "_annan", annan_url = "https://stat.example.org/api/v2")
    session$elapse(1000)
    expect_identical(webbplats()$base_url, "https://stat.example.org/api/v2/tables/")
  })
})

test_that("leverantör utan kända språk får inte språket \"NA\"", {
  old <- options(pxweb2rgui.backend = list(providers = function() data.frame(
    id = "x", name = "X", api_url = "https://x.invalid/api/v2/",
    tables_url = "https://x.invalid/api/v2/tables/", default_lang = NA_character_,
    languages = "NA", stringsAsFactors = FALSE)))
  on.exit(options(old), add = TRUE)
  l <- px_fallback_languages("https://x.invalid/api/v2/tables/")
  expect_equal(l$id, c("sv", "en"))
  expect_equal(attr(l, "default"), "sv")
})

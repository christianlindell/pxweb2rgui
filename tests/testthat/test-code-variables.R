# minimal json-stat2 metadata and data for an imagined table, used to test
# code_variables without network access

.cv_dimension <- function(label, codes, labels, show = NA) {
  ext <- list(elimination = TRUE)
  if (!is.na(show)) ext$show <- show
  list(
    label = label,
    category = list(
      index = stats::setNames(as.list(seq_along(codes) - 1L), codes),
      label = stats::setNames(as.list(labels), codes)
    ),
    extension = ext
  )
}

.cv_json <- function() {
  list(
    version = "2.0",
    class = "dataset",
    label = "Testtabell",
    id = c("Region", "Kon", "ContentsCode", "Tid"),
    size = c(2L, 2L, 1L, 2L),
    role = list(time = "Tid", metric = "ContentsCode", geo = "Region"),
    dimension = list(
      Region = .cv_dimension("region", c("00", "01"), c("Riket", "Stockholms län"), "code_value"),
      Kon = .cv_dimension("kön", c("1", "2"), c("män", "kvinnor")),
      ContentsCode = .cv_dimension("tabellinnehåll", "BE0101N1", "Folkmängd"),
      Tid = .cv_dimension("år", c("2023", "2024"), c("2023", "2024"))
    ),
    extension = list(px = list(tableid = "TAB0000")),
    value = as.list(1:8)
  )
}

.cv_get_data <- function(...) {
  meta <- .cv_json()
  meta$value <- NULL
  local_mocked_bindings(
    pxweb2_get_metadata = function(...) meta,
    .pxweb2_GET = function(url, ...) structure(list(url = url, status_code = 404L), class = "response"),
    .pxweb2_POST = function(...) {
      structure(
        list(
          url = "https://example.org",
          status_code = 200L,
          headers = list(`content-type` = "application/json; charset=utf-8"),
          content = charToRaw(enc2utf8(as.character(jsonlite::toJSON(.cv_json(), auto_unbox = TRUE))))
        ),
        class = "response"
      )
    },
    .package = "pxweb2rgui"
  )
  suppressMessages(pxr_pxweb2_get_data("TAB0000", query = list(Region = "*", Kon = "*", ContentsCode = "*", Tid = "*"),
                                   quiet = TRUE, ...))
}

test_that("code_variables translates labels, codes and positions", {
  vars <- tibble::tibble(code = c("Region", "Kon", "Tid"), label = c("region", "kön", "år"))
  f <- pxweb2rgui:::.pxweb2_resolve_code_variables
  expect_identical(f(vars, NULL), character(0))
  expect_identical(f(vars, c("kön", "region")), c("Kon", "Region"))
  expect_identical(f(vars, c("Region", "TID")), c("Region", "Tid"))
  expect_identical(f(vars, c(1, 3)), c("Region", "Tid"))
  expect_identical(f(vars, 2L), "Kon")
  expect_identical(f(vars, "*"), c("Region", "Kon", "Tid"))
  expect_error(f(vars, "kommun"), "kommun")
  expect_error(f(vars, 4), "position")
  expect_error(f(vars, 1.5), "position")
  expect_error(f(vars, TRUE), "code_variables")
  # an exact variable code wins over another variable's label
  vars2 <- tibble::tibble(code = c("A", "B"), label = c("B", "a"))
  expect_identical(f(vars2, "B"), "B")
  expect_identical(f(vars2, "b"), "A")
})

test_that("code_variables given as codes give the same columns", {
  skip_if_not_installed("rjstat")
  df <- .cv_get_data(code_variables = c("Region", "Kon"))
  expect_named(df, c("region_kod", "region", "kön_kod", "kön", "tabellinnehåll", "år", "value"))
  expect_identical(df$kön_kod[df$kön == "kvinnor"][1], "2")
})

test_that("harmonised names can be used in code_variables", {
  vars <- tibble::tibble(code = c("Kommun", "Tid"), label = c("kommun", "år"))
  f <- pxweb2rgui:::.pxweb2_harmonise_code_variables
  expect_identical(f(c("Region", "år"), vars, c(Region = "Kommun")), c("Kommun", "år"))
  expect_identical(f(1, vars, c(Region = "Kommun")), 1)
})

test_that("pxr_pxweb2_get_data returns only labels by default", {
  skip_if_not_installed("rjstat")
  df <- .cv_get_data()
  expect_named(df, c("region", "kön", "tabellinnehåll", "år", "value"))
  expect_setequal(unique(df$region), c("Riket", "Stockholms län"))
})

test_that("pxr_pxweb2_get_data adds <variable>_kod for the chosen variables", {
  skip_if_not_installed("rjstat")
  df <- .cv_get_data(code_variables = c("region", "kön"))
  expect_named(df, c("region_kod", "region", "kön_kod", "kön", "tabellinnehåll", "år", "value"))
  expect_identical(df$kön_kod[df$kön == "kvinnor"][1], "2")
  expect_identical(df$region_kod[df$region == "Riket"][1], "00")

  df_pos <- .cv_get_data(code_variables = c(3, 4))
  expect_named(df_pos, c("region", "kön", "tabellinnehåll_kod", "tabellinnehåll", "år_kod", "år", "value"))
  expect_identical(unique(df_pos$tabellinnehåll_kod), "BE0101N1")
})

test_that("the json-stat2 response is parsed once into labels and codes", {
  json <- as.character(jsonlite::toJSON(.cv_json(), auto_unbox = TRUE))
  p <- pxweb2rgui:::.pxweb2_parse_jsonstat2(json, code_dims = c("Region", "Kon"))
  expect_named(p$labels, c("region", "kön", "tabellinnehåll", "år", "value"))
  expect_named(p$codes, c("Region", "Kon"))
  expect_equal(nrow(p$labels), 8)
  # the last dimension varies fastest
  expect_identical(p$labels$år[1:2], c("2023", "2024"))
  expect_identical(p$labels$kön[1:4], c("män", "män", "kvinnor", "kvinnor"))
  expect_identical(p$labels$region[c(1, 5)], c("Riket", "Stockholms län"))
  expect_identical(p$codes$Region[c(1, 5)], c("00", "01"))
  expect_equal(p$labels$value, 1:8)
})

test_that("missing values and index given as an array are handled", {
  js <- .cv_json()
  js$dimension$Tid$category$index <- c("2023", "2024")
  js$value[[3]] <- NA
  json <- as.character(jsonlite::toJSON(js, auto_unbox = TRUE, na = "null"))
  p <- pxweb2rgui:::.pxweb2_parse_jsonstat2(json)
  expect_true(is.na(p$labels$value[3]))
  expect_identical(p$labels$år[1:2], c("2023", "2024"))
  expect_null(p$codes)
})

test_that("code columns are named after the response's labels when lang differs", {
  skip_if_not_installed("rjstat")
  meta <- .cv_json()
  meta$value <- NULL
  resp <- .cv_json()
  resp$dimension$Kon$label <- "sex"
  local_mocked_bindings(
    pxweb2_get_metadata = function(...) meta,
    .pxweb2_GET = function(url, ...) structure(list(url = url, status_code = 404L), class = "response"),
    .pxweb2_POST = function(...) {
      structure(
        list(url = "https://example.org", status_code = 200L,
             headers = list(`content-type` = "application/json; charset=utf-8"),
             content = charToRaw(enc2utf8(as.character(jsonlite::toJSON(resp, auto_unbox = TRUE))))),
        class = "response"
      )
    },
    .package = "pxweb2rgui"
  )
  df <- pxr_pxweb2_get_data("TAB0000", query = list(Region = "*", Kon = "*", ContentsCode = "*", Tid = "*"),
                        lang = "en", code_variables = "kön", quiet = TRUE)
  expect_named(df, c("region", "sex_kod", "sex", "tabellinnehåll", "år", "value"))
  df2 <- pxr_pxweb2_get_data("TAB0000", query = list(Region = "*", Kon = "*", ContentsCode = "*", Tid = "*"),
                         lang = "en", code_variables = "Kon", quiet = TRUE)
  expect_named(df2, c("region", "sex_kod", "sex", "tabellinnehåll", "år", "value"))
})

test_that("'**', agg_ and from()/to()/range() survive the value check", {
  vvl <- list(Tid = tibble::tibble(code = c("2021", "2022", "2023", "2024"), type = "Variable"),
              Region = tibble::tibble(code = c("00", "01"), type = "Variable"))
  q <- list(selection = list(
    list(variableCode = "Tid", valueCodes = list("from(2022)")),
    list(variableCode = "Region", valueCodes = list("**"))
  ))
  r <- pxweb2rgui:::.pxweb2_resolve_api_wildcards(q, vvl)
  expect_identical(unlist(r$selection[[1]]$valueCodes), c("2022", "2023", "2024"))
  s <- pxweb2rgui:::.pxweb2_sanitize_query_values(r, vvl)
  expect_identical(unlist(s$selection[[2]]$valueCodes), "**")

  span <- function(x) {
    q$selection[[1]]$valueCodes <- list(x)
    unlist(pxweb2rgui:::.pxweb2_resolve_api_wildcards(q, vvl)$selection[[1]]$valueCodes)
  }
  expect_identical(span("to(2022)"), c("2021", "2022"))
  expect_identical(span("range(2024, 2022)"), c("2022", "2023", "2024"))
  expect_identical(span("range(2022,2023)"), c("2022", "2023"))

  q$selection[[1]]$valueCodes <- list("2022")
  q$selection[[2]]$valueCodes <- list("agg_RegionA-region_2")
  expect_identical(
    unlist(pxweb2rgui:::.pxweb2_sanitize_query_values(q, vvl)$selection[[2]]$valueCodes),
    "agg_RegionA-region_2"
  )
})

test_that("DeSO and RegSO rows get a municipality name, with the general code list as fallback", {
  df <- tibble::tibble(
    region_kod = c("0114A0010", "0114R001", "2080R002"),
    region = c("0114A0010", "Upplands Väsby centrala", "Falun norra"),
    kommun_kod = c("0114", "0114", "2080"),
    kommun = NA_character_
  )
  local_mocked_bindings(
    pxweb2_get_codelist = function(id, ...) {
      expect_identical(id, "vs_RegionKommun07")
      tibble::tibble(code = c("0114", "2080"), label = c("0114 Upplands Väsby", "2080 Falun"))
    },
    .package = "pxweb2rgui"
  )
  out <- pxweb2rgui:::.pxweb2_fill_deso_municipality_from_metadata(
    df, metadata = list(dimension = list()), base_url = "https://example.org/api/v2/tables/"
  )
  expect_identical(out$kommun, c("Upplands Väsby", "Upplands Väsby", "Falun"))
})

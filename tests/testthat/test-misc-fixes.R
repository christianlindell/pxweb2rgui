fake_response <- function(url, status, body) {
  httr:::response(url = url, status_code = status, content = charToRaw(body),
                  headers = list(`Content-Type` = "application/json"))
}

test_that("pxweb2_search_tables follows page$totalPages and asks for no extra page", {
  pages_asked <- integer()
  testthat::local_mocked_bindings(
    .pxweb2_get_config = function(base_url) list(maxDataCells = 150000),
    .pxweb2_GET = function(url, ..., query = list()) {
      pn <- query$pageNumber
      pages_asked <<- c(pages_asked, pn)
      if (pn > 2) return(fake_response(url, 400L, '{"title":"bad page"}'))
      ids <- sprintf("TAB%d", (pn - 1) * 2 + 1:2)
      fake_response(url, 200L, jsonlite::toJSON(list(
        tables = data.frame(id = ids, label = ids),
        page = list(pageNumber = pn, pageSize = 2, totalElements = 4, totalPages = 2)),
        auto_unbox = TRUE))
    },
    .package = "pxweb2rgui")
  res <- pxweb2_search_tables(base_url = "https://x.invalid/api/v2/", lang = "sv", pageSize = 2)
  expect_equal(res$id, sprintf("TAB%d", 1:4))
  expect_equal(pages_asked, 1:2)
})

test_that("pxweb2_search_tables keeps paging when the API caps pageSize", {
  testthat::local_mocked_bindings(
    .pxweb2_get_config = function(base_url) list(maxDataCells = 150000),
    .pxweb2_GET = function(url, ..., query = list()) {
      pn <- query$pageNumber
      ids <- sprintf("TAB%d", (pn - 1) * 2 + 1:2)   # 2 per page although 200 asked
      fake_response(url, 200L, jsonlite::toJSON(list(
        tables = data.frame(id = ids, label = ids),
        page = list(pageNumber = pn, pageSize = 2, totalElements = 6, totalPages = 3)),
        auto_unbox = TRUE))
    },
    .package = "pxweb2rgui")
  res <- pxweb2_search_tables(base_url = "https://x.invalid/api/v2/", lang = "sv")
  expect_equal(nrow(res), 6)
})

test_that("pxweb2_table_updated accepts a metadata object", {
  asked <- NULL
  testthat::local_mocked_bindings(
    pxweb2_get_metadata = function(table_id, ...) { asked <<- table_id; list(updated = "2026-01-02T00:00:00Z") },
    .package = "pxweb2rgui")
  meta <- list(extension = list(px = list(tableid = "TAB123")), id = c("Region", "Tid"))
  expect_equal(pxweb2_table_updated(meta), "2026-01-02T00:00:00Z")
  expect_equal(asked, "TAB123")
})

test_that("pxweb2_GET_url_to_px keeps code lists, aggregations and language", {
  x <- pxweb2_GET_url_to_px(paste0(
    "https://statistikdatabasen.scb.se/api/v2/tables/TAB5457/data?lang=en",
    "&valuecodes%5BContentsCode%5D=*&valuecodes[Region]=A-01,A-02",
    "&codelist[Region]=agg_RegionA-region_2"))
  expect_equal(x$table, "TAB5457")
  expect_equal(x$query, list(ContentsCode = "*", Region = c("A-01", "A-02")))
  expect_equal(x$lang, "en")
  expect_equal(x$include_aggregations, c(Region = "agg_RegionA-region_2"))

  y <- pxweb2_GET_url_to_px(
    "https://statistikdatabasen.scb.se/api/v2/tables/TAB5457/data?valuecodes[Region]=21&codelist[Region]=vs_A_L%C3%A4nRMI")
  expect_null(y$include_aggregations)
  expect_null(y$lang)
  expect_equal(unname(y$codelist), "vs_A_LänRMI")
})

test_that("a failing municipality code list does not stop the data fetch", {
  testthat::local_mocked_bindings(
    .pxweb2_municipality_key_from_metadata = function(...) stop("HTTP 500"),
    pxweb2_get_codelist = function(...) tibble::tibble(code = "0114", label = "Upplands Väsby"),
    .package = "pxweb2rgui")
  df <- data.frame(region_code = "0114A0010", kommun_code = "0114", kommun = NA_character_)
  out <- pxweb2rgui:::.pxweb2_fill_deso_municipality_from_metadata(df, metadata = list(),
                                                               base_url = "https://x.invalid/api/v2/tables/")
  expect_equal(out$kommun, "Upplands Väsby")
})

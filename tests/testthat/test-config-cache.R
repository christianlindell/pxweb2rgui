test_that("config falls back to Statistics Sweden's limits", {
  cfg <- pxweb2rgui:::.pxweb2_normalise_config(NULL)
  expect_equal(cfg$maxDataCells, 150000)
  expect_identical(cfg$maxCallsPerTimeWindow, 30L)
  expect_equal(cfg$timeWindow, 10)

  cfg <- pxweb2rgui:::.pxweb2_normalise_config(list(maxDataCells = 200000, maxCallsPerTimeWindow = "abc",
                                                  timeWindow = 0, license = "CC0"))
  expect_equal(cfg$maxDataCells, 200000)
  expect_identical(cfg$maxCallsPerTimeWindow, 30L)
  expect_equal(cfg$timeWindow, 10)
  expect_identical(cfg$license, "CC0")
})

test_that("config is read from /config once and sets the rate limiter", {
  pxweb2_clear_cache()
  on.exit({ pxweb2_clear_cache(); pxweb2rgui:::.pxweb2_set_rate_limit(30L, 10) })
  urls <- character()
  local_mocked_bindings(
    .pxweb2_GET = function(url, ...) {
      urls <<- c(urls, url)
      structure(list(url = url, status_code = 200L,
                     headers = structure(list(`content-type` = "application/json; charset=utf-8"), class = c("insensitive", "list")),
                     content = charToRaw('{"maxDataCells": 250000, "maxCallsPerTimeWindow": 40, "timeWindow": 20}')),
                class = "response")
    },
    .package = "pxweb2rgui"
  )
  cfg <- pxweb2_get_config("https://example.org/api/v2/tables/")
  cfg2 <- pxweb2_get_config("https://example.org/api/v2/tables/")
  expect_identical(urls, "https://example.org/api/v2/config")
  expect_equal(cfg$maxDataCells, 250000)
  expect_identical(cfg2$maxCallsPerTimeWindow, 40L)
  expect_identical(pxweb2rgui:::.pxweb2_rate_state$max_calls, 40L)
  expect_equal(pxweb2rgui:::.pxweb2_rate_state$window, 20)
})

test_that("metadata is cached, and table_updated always asks the API", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  n <- 0
  local_mocked_bindings(
    .pxweb2_GET = function(url, ...) {
      # /config (read for the default language) is not counted
      if (grepl("/config$", url)) return(structure(list(url = url, status_code = 404L), class = "response"))
      n <<- n + 1
      structure(list(url = url, status_code = 200L,
                     headers = structure(list(`content-type` = "application/json; charset=utf-8"), class = c("insensitive", "list")),
                     content = charToRaw(paste0('{"updated": "2026-10-0', n, 'T06:00:00Z"}'))),
                class = "response")
    },
    .package = "pxweb2rgui"
  )
  m1 <- pxweb2_get_metadata("TAB1", base_url = "https://example.org/api/v2/tables/")
  m2 <- pxweb2_get_metadata("TAB1", base_url = "https://example.org/api/v2/tables/")
  expect_identical(n, 1)
  expect_identical(m1, m2)
  expect_identical(pxweb2_table_updated("TAB1", base_url = "https://example.org/api/v2/tables/"),
                   "2026-10-02T06:00:00Z")

  old <- options(pxweb2r.cache_minutes = 0)
  on.exit(options(old), add = TRUE)
  pxweb2_get_metadata("TAB1", base_url = "https://example.org/api/v2/tables/")
  expect_identical(n, 3)
})

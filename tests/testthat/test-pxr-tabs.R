fake_tables <- function() data.frame(
  id = c("TAB1", "TAB2", "TAB3", "TAB4"),
  label = c("Folkmängd efter region", "Födda efter kön", "Döda efter ålder",
            "Bostadsbestånd"),
  updated = c("2026-05-12T06:00:00Z", "2026-04-30T06:00:00Z",
              "2026-06-01T06:00:00Z", NA),
  stringsAsFactors = FALSE)

mock_search <- function(env = new.env()) {
  function(query = NULL, base_url = NULL, lang = NULL, pastDays = NULL, ...) {
    env$base_url <- base_url; env$lang <- lang; env$pastDays <- pastDays
    fake_tables()
  }
}

test_that("pxr_get_all_tabs returns tabellid, tab_name and updated", {
  env <- new.env()
  local_mocked_bindings(pxweb2_search_tables = mock_search(env))
  res <- pxr_get_all_tabs("scb")
  expect_s3_class(res, "data.frame")
  expect_named(res, c("tabellid", "tab_name", "updated"))
  expect_equal(res$tabellid, c("TAB1", "TAB2", "TAB3", "TAB4"))
  expect_s3_class(res$updated, "Date")
  expect_equal(env$base_url, pxweb2_base_url("scb"))
  expect_null(env$pastDays)
})

test_that("updated_since keeps tables updated on or after the date", {
  env <- new.env()
  local_mocked_bindings(pxweb2_search_tables = mock_search(env))
  res <- pxr_get_all_tabs("scb", updated_since = "2026-05-12", lang = "en")
  expect_equal(res$tabellid, c("TAB1", "TAB3"))
  expect_equal(env$lang, "en")
  expect_true(env$pastDays >= as.integer(Sys.Date() - as.Date("2026-05-12")))
  expect_error(pxr_get_all_tabs("scb", updated_since = "12/05/2026"), "year-month-day")
  expect_error(pxr_get_all_tabs("scb", updated_since = "2026-13-40"), "year-month-day")
})

test_that("provider can be an id, a name or a URL, in any case", {
  env <- new.env()
  local_mocked_bindings(pxweb2_search_tables = mock_search(env))
  pxr_get_all_tabs("SCB")
  expect_equal(env$base_url, pxweb2_base_url("scb"))
  pxr_get_all_tabs(toupper(pxweb2_providers()$name[2]))
  expect_equal(env$base_url, pxweb2_base_url("ssb"))
  pxr_get_all_tabs("https://x.invalid/api/v2/")
  expect_match(env$base_url, "^https://x.invalid/api/v2")
  expect_error(pxr_get_all_tabs("finns_inte"), "Unknown provider")
})

test_that("pxr_search_tabs keeps names containing any search text", {
  local_mocked_bindings(pxweb2_search_tables = mock_search())
  expect_equal(pxr_search_tabs("scb", search_str = "FOLKMÄNGD")$tabellid, "TAB1")
  expect_equal(pxr_search_tabs("scb", search_str = c("födda", "döda"))$tabellid,
               c("TAB2", "TAB3"))
  expect_equal(pxr_search_tabs("scb", updated_since = "2026-05-01",
                               search_str = c("födda", "döda"))$tabellid, "TAB3")
  res <- pxr_search_tabs("scb", search_str = "finns inte")
  expect_equal(nrow(res), 0)
  expect_named(res, c("tabellid", "tab_name", "updated"))
  expect_error(pxr_search_tabs("scb"), "search_str")
})

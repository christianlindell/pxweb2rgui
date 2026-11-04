fake_meta <- function() list(
  id = list("Region", "Kon"),
  dimension = list(
    Kon = list(label = "kön", category = list(
      index = list("1" = 0, "2" = 1), label = list("1" = "män", "2" = "kvinnor"))),
    Region = list(label = "region", category = list(
      index = list("00" = 0, "20" = 1, "2021" = 2, "2080" = 3, "0180" = 4),
      label = list("00" = "Riket", "20" = "Dalarnas län", "2021" = "Vansbro",
                   "2080" = "Falun", "0180" = "Stockholm")))
  )
)

test_that("pxr_get_labels filters and swaps to codes", {
  m <- fake_meta()
  expect_equal(pxr_get_labels("scb", m, "Region"),
               c("Riket", "Dalarnas län", "Vansbro", "Falun", "Stockholm"))
  expect_equal(pxr_get_labels("scb", m, "region", ends_with = "län"), "Dalarnas län")
  expect_equal(pxr_get_labels("scb", m, "Region", starts_with = c("F", "V")), c("Vansbro", "Falun"))
  expect_equal(pxr_get_labels("scb", m, "Region", n_chars = 5), c("Riket", "Falun"))
  expect_equal(pxr_get_labels("scb", m, "Region", contains = "o", not_contains = "Stock"), "Vansbro")
  expect_equal(pxr_get_labels("scb", m, "Region", n_chars = 5, to_codes = TRUE), c("00", "2080"))
  expect_equal(pxr_get_labels("scb", m, "Region", starts_with = "X"), character(0))
  expect_equal(pxr_get_labels("scb", m, "kön"), c("män", "kvinnor"))
})

test_that("pxr_get_codes filters and swaps to labels", {
  m <- fake_meta()
  expect_equal(pxr_get_codes("scb", m, "Region", starts_with = "20", n_chars = 4),
               c("2021", "2080"))
  expect_equal(pxr_get_codes("scb", m, "Region", starts_with = "20", n_chars = 4,
                             to_labels = TRUE), c("Vansbro", "Falun"))
  expect_equal(pxr_get_codes("scb", m, "Region", ends_with = "0", not_contains = "20"),
               c("00", "0180"))
  expect_error(pxr_get_codes("scb", m, "Alder"), "does not exist")
})

test_that("pxr_get_all_* return named lists in table order", {
  m <- fake_meta()
  l <- pxr_get_all_labels("scb", m)
  expect_named(l, c("Region", "Kon"))
  expect_equal(l$Kon, c("män", "kvinnor"))
  expect_equal(pxr_get_all_codes("scb", m)$Region, c("00", "20", "2021", "2080", "0180"))
})

test_that("provider is resolved to a base URL and passed on", {
  seen <- NULL
  local_mocked_bindings(pxweb2_get_metadata = function(table_id, base_url, lang = NULL, ...) {
    seen <<- list(table_id = table_id, base_url = base_url, lang = lang); fake_meta()
  })
  expect_equal(pxr_get_all_codes("ssb", "07459", lang = "en")$Kon, c("1", "2"))
  expect_equal(seen$table_id, "07459")
  expect_match(seen$base_url, "ssb.no")
  expect_equal(seen$lang, "en")
  expect_match(pxr_get_codes("https://example.org/api/v2", "X1", "Kon")[1], "1")
  expect_equal(seen$base_url, "https://example.org/api/v2/tables/")
})

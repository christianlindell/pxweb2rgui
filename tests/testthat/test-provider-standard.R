test_that("provider is scb by default in every pxr_ function that has it", {
  for (f in list(pxr_get_all_tabs, pxr_search_tabs, pxr_get_labels, pxr_get_codes,
                 pxr_get_all_labels, pxr_get_all_codes)) {
    expect_identical(formals(f)$provider, "scb")
  }
})

test_that("leaving provider out uses Statistics Sweden", {
  seen <- new.env()
  local_mocked_bindings(
    pxweb2_get_metadata = function(table_id, base_url, lang = NULL, ...) {
      seen$base_url <- base_url
      list(id = list("Kon"), dimension = list(Kon = list(label = "kön", category = list(
        index = list("1" = 0, "2" = 1), label = list("1" = "män", "2" = "kvinnor")))))
    },
    pxweb2_search_tables = function(query = NULL, base_url = NULL, ...) {
      seen$tabs_url <- base_url
      data.frame(id = "TAB1", label = "Folkmängd", updated = "2026-05-12T06:00:00Z")
    }
  )
  scb <- pxweb2_base_url("scb")
  expect_identical(pxr_get_all_codes(tab = "T")$Kon, c("1", "2"))
  expect_identical(seen$base_url, scb)
  expect_identical(pxr_get_all_labels(tab = "T")$Kon, c("män", "kvinnor"))
  expect_identical(pxr_get_labels(tab = "T", var = "Kon", starts_with = "k"), "kvinnor")
  expect_identical(pxr_get_codes(tab = "T", var = "Kon", n_chars = 1), c("1", "2"))
  expect_identical(pxr_get_all_tabs()$tabellid, "TAB1")
  expect_identical(seen$tabs_url, scb)
  expect_identical(pxr_search_tabs(search_str = "folk")$tabellid, "TAB1")

  # ett namn som första argument fungerar som förut
  expect_identical(pxr_get_all_codes("ssb", "T")$Kon, c("1", "2"))
  expect_match(seen$base_url, "ssb.no")
})

test_that("tab and var are still required", {
  expect_error(pxr_get_all_codes(), "tab must be supplied")
  expect_error(pxr_get_codes(tab = list(id = list(), dimension = list())), "var must be")
  expect_error(pxr_search_tabs(), "search_str")
})

test_that("the first tab shows the name to give as provider", {
  old <- options(pxweb2rgui.backend = mock_backend(), pxweb2rgui.cache_file = tempfile(fileext = ".rds"))
  on.exit(options(old))
  expect_match(as.character(pxweb2rgui:::px_app_ui()), "provider_namn", fixed = TRUE)

  shiny::testServer(px_app_server(), {
    session$setInputs(leverantor = "scb", sprak = "sv")
    expect_match(output$provider_namn$html, ">scb</code>", fixed = TRUE)
    expect_match(output$provider_namn$html, "Namn att ange som provider", fixed = TRUE)
    session$setInputs(leverantor = "ssb")
    expect_match(output$provider_namn$html, ">ssb</code>", fixed = TRUE)
    # en egen webbplats anges med sin adress
    session$setInputs(leverantor = "_annan", annan_url = "https://x.example/api/v2")
    session$elapse(1000)
    expect_match(output$provider_namn$html, ">https://x.example/api/v2/tables/</code>", fixed = TRUE)
  })
})

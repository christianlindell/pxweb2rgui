test_that("addin: 'Infoga koden' skriver den genererade koden", {
  old <- options(pxweb2rgui.backend = mock_backend(),
                 pxweb2rgui.cache_file = tempfile(fileext = ".rds"))
  on.exit(options(old))
  inserted <- NULL
  local_mocked_bindings(px_insert_code = function(code) inserted <<- code)
  local_mocked_bindings(stopApp = function(...) invisible(NULL), .package = "shiny")

  testServer(px_app_server(addin = TRUE), {
    session$setInputs(sprak = "sv")
    session$setInputs(vald_tabell = "TAB6104")
    session$setInputs(t1_varden_1 = c("00", "01"))
    session$setInputs(infoga_kod = 1)
    expect_match(inserted, 'table = "TAB6104"', fixed = TRUE)
    expect_match(inserted, 'Region = c("00", "01")', fixed = TRUE)
  })
})

test_that("addin-gränssnittet har knapparna för att infoga och stänga", {
  html <- as.character(px_app_ui(addin = TRUE))
  expect_match(html, "infoga_kod")
  expect_match(html, 'id="stang"')
  expect_false(grepl("infoga_kod", as.character(px_app_ui(addin = FALSE))))
})

test_that("addins.dcf pekar på en exporterad funktion", {
  dcf <- read.dcf(system.file("rstudio", "addins.dcf", package = "pxweb2rgui"))
  expect_equal(unname(dcf[, "Binding"]), "pxweb2_gui_addin")
  expect_true("pxweb2_gui_addin" %in% getNamespaceExports("pxweb2rgui"))
})

test_that("Positron känns igen och appen stoppas bara en gång", {
  withr_env <- function(vars, code) {
    old <- Sys.getenv(names(vars), unset = NA)
    do.call(Sys.setenv, as.list(vars))
    on.exit(for (n in names(old)) if (is.na(old[[n]])) Sys.unsetenv(n) else do.call(Sys.setenv, stats::setNames(list(old[[n]]), n)))
    force(code)
  }
  withr_env(c(POSITRON = "1"), expect_true(px_is_positron()))
  withr_env(c(POSITRON = "", POSITRON_VERSION = ""), expect_false(px_is_positron()))

  old <- options(pxweb2rgui.backend = mock_backend(),
                 pxweb2rgui.cache_file = tempfile(fileext = ".rds"))
  on.exit(options(old))
  n <- 0
  local_mocked_bindings(px_stop_app = function(...) n <<- n + 1)
  testServer(px_app_server(addin = TRUE), {
    session$setInputs(stang = 1)
    session$setInputs(stang = 2)
  })
  expect_equal(n, 1)
})

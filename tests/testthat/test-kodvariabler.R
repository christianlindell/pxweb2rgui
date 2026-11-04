test_that("koden får code_variables bara när kodkolumner valts", {
  query <- list(Region = "00", Tid = "9999")
  expect_false(grepl("code_variables", px_query_code("TAB1", "t", query)))
  code <- px_query_code("TAB1", "t", query, code_variables = c("region", "år"))
  expect_match(code, 'code_variables = c("region", "år")', fixed = TRUE)
  call <- parse(text = code)[[2]][[3]]
  expect_equal(eval(call$code_variables), c("region", "år"))
})

test_that("valda kodvariabler hamnar i koden och i hämtningen", {
  old <- options(pxweb2rgui.backend = mock_backend(),
                 pxweb2rgui.cache_file = tempfile(fileext = ".rds"))
  on.exit(options(old))

  testServer(px_app_server(), {
    session$setInputs(sprak = "sv")
    session$setInputs(vald_tabell = "TAB6104")
    v <- tabell()$variables
    expect_false(grepl("code_variables", fraga()$code))

    # en eliminerad variabel ger ingen kodkolumn även om den är ikryssad
    session$setInputs(t1_varden_1 = c("00", "01"), t1_varden_2 = character(0),
                      t1_varden_2_andrad = 1)
    session$setInputs(t1_kodvariabler_0 = v$code[1:2])
    expect_identical(fraga()$code_variables, v$code[1])
    expect_match(fraga()$code, paste0('code_variables = "', v$code[1], '"'), fixed = TRUE)
    # rutorna visar namnen men har koderna som värden
    expect_identical(names(kod_valbara())[1], v$label[1])
    expect_identical(unname(kod_valbara())[1], v$code[1])

    session$setInputs(hamta_data = 1)
    d <- data_resultat()
    expect_true(paste0(v$label[1], "_kod") %in% names(d))
    expect_identical(match(paste0(v$label[1], "_kod"), names(d)) + 1L, match(v$label[1], names(d)))
  })
})

test_that("äldre pxweb2r utan code_variables ger ett begripligt fel", {
  b <- list(get_data = function(...) list(...))
  expect_named(px_get_data(b, table = "X"), "table")
  expect_named(px_get_data(b, table = "X", code_variables = "region"), c("table", "code_variables"))
})

test_that("koden för kodvariabler är ren ASCII även när namnen har å, ä, ö", {
  old <- options(pxweb2rgui.backend = mock_backend(),
                 pxweb2rgui.cache_file = tempfile(fileext = ".rds"))
  on.exit(options(old))
  testServer(px_app_server(), {
    session$setInputs(sprak = "sv")
    session$setInputs(vald_tabell = "TAB6104")
    v <- tabell()$variables
    session$setInputs(t1_varden_1 = c("00", "01"), t1_varden_2 = c("1", "2"))
    session$setInputs(t1_kodvariabler_0 = c("Region", "Kon"))
    expect_identical(fraga()$code_variables, c("Region", "Kon"))
    rad <- grep("code_variables", strsplit(fraga()$code, "\n")[[1]], value = TRUE)
    expect_identical(rad, '  code_variables = c("Region", "Kon"),')
    expect_true(all(utf8ToInt(rad) < 128))
    session$setInputs(hamta_data = 1)
    expect_true(all(c("region_kod", "kön_kod") %in% names(data_resultat())))
  })
})

test_that("kryssrutan clean_names ändrar koden och kolumnnamnen i appen", {
  old <- options(pxweb2rgui.backend = mock_backend(),
                 pxweb2rgui.cache_file = tempfile(fileext = ".rds"))
  on.exit(options(old))
  local_mocked_bindings(px_clean_names = function(d) {
    names(d) <- paste0("ren_", seq_along(d)); d
  })
  testServer(px_app_server(), {
    session$setInputs(sprak = "sv")
    session$setInputs(vald_tabell = "TAB6104")
    session$setInputs(t1_varden_1 = c("00", "01"), t1_varden_2 = c("1", "2"))
    session$setInputs(clean_names = FALSE)
    expect_false(grepl("clean_names", fraga()$code))
    session$setInputs(hamta_data = 1)
    raa <- names(data_resultat())
    visad <- names(data_visad())

    session$setInputs(clean_names = TRUE)
    expect_match(fraga()$code, "dataset_df <- janitor::clean_names(dataset_df)", fixed = TRUE)
    expect_identical(names(data_visad()), paste0("ren_", seq_along(visad)))
    # hämtat data ändras inte, så rutan kan kryssas ur igen
    expect_identical(names(data_resultat()), raa)
  })
})

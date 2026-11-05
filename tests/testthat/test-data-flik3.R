# Hämtat data i flik 3: raderas när frågan ändras, och rutorna för kodkolumner och
# kolumnnamn ändrar tabellen utan ny hämtning. Med testbackenden, som räknar hämtningarna.

data_backend <- function() {
  b <- mock_backend()
  rakna <- new.env()
  rakna$n <- 0
  rakna$args <- NULL
  gd <- b$get_data
  b$get_data <- function(...) {
    rakna$n <- rakna$n + 1
    rakna$args <- list(...)
    gd(...)
  }
  list(backend = b, rakna = rakna)
}

data_options <- function(b) {
  options(pxweb2rgui.backend = b, pxweb2rgui.cache_file = tempfile(fileext = ".rds"))
}

test_that("hämtningen ber om kodkolumner för alla variabler i uttaget, men koden bara de valda", {
  x <- data_backend()
  old <- data_options(x$backend)
  on.exit(options(old))
  testServer(px_app_server(), {
    session$setInputs(sprak = "sv")
    session$setInputs(vald_tabell = "TAB6104")
    # en eliminerad variabel är inte med i uttaget
    session$setInputs(t1_varden_1 = c("00", "01"), t1_varden_2 = character(0), t1_varden_2_andrad = 1)
    session$setInputs(hamta_data = 1)
    expect_identical(x$rakna$args$code_variables, unname(fraga()$included))
    expect_false(tabell()$variables$code[2] %in% x$rakna$args$code_variables)
    expect_false(grepl("code_variables", fraga()$code))
    expect_identical(fraga()$code_variables, character())
  })
})

test_that("kodkolumner och kolumnnamn ändras direkt, åt båda hållen, utan ny hämtning", {
  x <- data_backend()
  old <- data_options(x$backend)
  on.exit(options(old))
  local_mocked_bindings(px_clean_names = function(d) {
    names(d) <- tolower(paste0("ren_", names(d))); d
  })
  testServer(px_app_server(), {
    session$setInputs(sprak = "sv")
    session$setInputs(vald_tabell = "TAB6104")
    session$setInputs(t1_varden_1 = c("00", "01"), t1_varden_2 = c("1", "2"))
    session$setInputs(hamta_data = 1)
    expect_identical(x$rakna$n, 1)
    raa <- data_resultat()
    region <- paste0(tabell()$variables$label[1], "_code")
    kon <- paste0(tabell()$variables$label[2], "_code")
    # utan valda rutor: inga kodkolumner visas, trots att de är hämtade
    expect_true(all(c(region, kon) %in% names(raa)))
    expect_false(any(grepl("_code$", names(data_visad()))))
    utan <- names(data_visad())

    # på: kolumnen visas, före sin klartextkolumn
    session$setInputs(t1_kodvariabler_0 = "Region")
    expect_true(region %in% names(data_visad()))
    expect_false(kon %in% names(data_visad()))
    expect_identical(match(region, names(data_visad())) + 1L,
                     match(tabell()$variables$label[1], names(data_visad())))
    expect_match(fraga()$code, 'code_variables = "Region"', fixed = TRUE)
    session$setInputs(t1_kodvariabler_0 = c("Region", "Kon"))
    expect_true(all(c(region, kon) %in% names(data_visad())))

    # av
    session$setInputs(t1_kodvariabler_0 = character(0))
    expect_identical(names(data_visad()), utan)
    expect_false(grepl("code_variables", fraga()$code))

    # kolumnnamn på och av
    session$setInputs(clean_names = TRUE)
    expect_identical(names(data_visad()), tolower(paste0("ren_", utan)))
    expect_match(fraga()$code, "janitor::clean_names", fixed = TRUE)
    session$setInputs(clean_names = FALSE)
    expect_identical(names(data_visad()), utan)

    # båda samtidigt, och i tur och ordning tillbaka
    session$setInputs(clean_names = TRUE, t1_kodvariabler_0 = "Kon")
    expect_true(tolower(paste0("ren_", kon)) %in% names(data_visad()))
    expect_false(tolower(paste0("ren_", region)) %in% names(data_visad()))
    session$setInputs(t1_kodvariabler_0 = character(0))
    session$setInputs(clean_names = FALSE)
    expect_identical(names(data_visad()), utan)

    # rådatat är orört, inget raderades och inget hämtades på nytt
    expect_identical(data_resultat(), raa)
    expect_identical(x$rakna$n, 1)
    expect_false(data_inaktuell())
  })
})

test_that("nedladdningen ger det som visas", {
  x <- data_backend()
  old <- data_options(x$backend)
  on.exit(options(old))
  testServer(px_app_server(), {
    session$setInputs(sprak = "sv", format = "txt")
    session$setInputs(vald_tabell = "TAB6104")
    session$setInputs(t1_varden_1 = c("00", "01"), t1_varden_2 = c("1", "2"))
    session$setInputs(hamta_data = 1)
    rubrik <- function() {
      fil <- output$ladda_ner_data
      strsplit(gsub('"', "", readLines(fil, n = 1, encoding = "UTF-8")), "[\t,;]")[[1]]
    }
    expect_identical(rubrik(), names(data_visad()))
    expect_false(any(grepl("_code$", rubrik())))
    session$setInputs(t1_kodvariabler_0 = "Region")
    expect_true(paste0(tabell()$variables$label[1], "_code") %in% rubrik())
    session$setInputs(t1_kodvariabler_0 = character(0))
    expect_false(any(grepl("_code$", rubrik())))
  })
})

test_that("datat raderas när valda värden eller senaste perioden ändras", {
  x <- data_backend()
  old <- data_options(x$backend)
  on.exit(options(old))
  testServer(px_app_server(), {
    session$setInputs(sprak = "sv")
    session$setInputs(vald_tabell = "TAB6104")
    # innan något hämtats är det inget som är inaktuellt
    session$setInputs(t1_varden_1 = c("00", "01"), t1_varden_2 = c("1", "2"))
    expect_match(as.character(output$data_status$html), "Ingen data hämtad ännu", fixed = TRUE)

    session$setInputs(hamta_data = 1)
    expect_false(is.null(data_resultat()))
    expect_match(as.character(output$data_status$html), "rader", fixed = TRUE)
    expect_match(as.character(output$data_tabell_ruta$html), "data_tabell", fixed = TRUE)

    # samma värden igen raderar inget
    session$setInputs(t1_varden_1 = c("00", "01"))
    expect_false(is.null(data_resultat()))

    session$setInputs(t1_varden_1 = "00")
    session$flushReact()
    expect_null(data_resultat())
    expect_null(data_visad())
    expect_true(data_inaktuell())
    expect_match(as.character(output$data_status$html), "Valen har ändrats", fixed = TRUE)
    expect_error(output$data_tabell)
    expect_error(output$data_tabell_ruta)

    # hämtas på nytt: texten försvinner
    session$setInputs(hamta_data = 2)
    expect_false(is.null(data_resultat()))
    expect_false(data_inaktuell())
    expect_match(as.character(output$data_status$html), "rader", fixed = TRUE)

    # senaste perioden
    i <- match("time", tabell()$variables$role)
    args <- stats::setNames(list(TRUE), paste0("t1_varden_", i, "_senaste"))
    do.call(session$setInputs, args)
    session$flushReact()
    expect_null(data_resultat())
    expect_true(data_inaktuell())
  })
})

test_that("datat raderas när tabell, språk eller leverantör ändras", {
  x <- data_backend()
  old <- data_options(x$backend)
  on.exit(options(old))
  testServer(px_app_server(), {
    session$setInputs(sprak = "sv")
    session$setInputs(vald_tabell = "TAB6104")
    session$setInputs(hamta_data = 1)
    expect_false(is.null(data_resultat()))
    # annan tabell
    session$setInputs(vald_tabell = "TAB5444")
    session$flushReact()
    expect_null(data_resultat())
    expect_true(data_inaktuell())
    expect_match(as.character(output$data_status$html), "Valen har ändrats", fixed = TRUE)

    # flera tabeller
    session$setInputs(hamta_data = 2)
    expect_false(is.null(data_resultat()))
    session$setInputs(vald_tabell = c("TAB5444", "TAB6104"))
    session$flushReact()
    expect_null(data_resultat())

    # ingen tabell
    session$setInputs(vald_tabell = "TAB6104")
    session$setInputs(hamta_data = 3)
    expect_false(is.null(data_resultat()))
    session$setInputs(vald_tabell = "")
    session$flushReact()
    expect_null(data_resultat())
    expect_true(data_inaktuell())

    # språk
    session$setInputs(vald_tabell = "TAB6104")
    session$setInputs(hamta_data = 4)
    expect_false(is.null(data_resultat()))
    session$setInputs(sprak = "en")
    session$flushReact()
    expect_null(data_resultat())
    expect_null(tabell())
  })
})

test_that("indelning raderar datat", {
  x <- data_backend()
  old <- data_options(x$backend)
  on.exit(options(old))
  testServer(px_app_server(), {
    session$setInputs(sprak = "sv")
    session$setInputs(vald_tabell = "TAB6104")
    session$setInputs(hamta_data = 1)
    expect_false(is.null(data_resultat()))
    session$setInputs(px_indelning = list(target = "t1_varden_1", codelist = "vs_RegionLän07"))
    session$flushReact()
    expect_null(data_resultat())
    expect_true(data_inaktuell())
  })
})

test_that("ett misslyckat försök raderas också när valen ändras", {
  b <- mock_backend()
  b$get_data <- function(...) stop("Servern svarade inte.")
  old <- data_options(b)
  on.exit(options(old))
  testServer(px_app_server(), {
    session$setInputs(sprak = "sv")
    session$setInputs(vald_tabell = "TAB6104")
    session$setInputs(t1_varden_1 = c("00", "01"))
    session$setInputs(hamta_data = 1)
    expect_match(as.character(output$data_status$html), "Hämtningen misslyckades", fixed = TRUE)
    session$setInputs(t1_varden_1 = "00")
    session$flushReact()
    expect_null(data_fel())
    expect_match(as.character(output$data_status$html), "Valen har ändrats", fixed = TRUE)
  })
})

test_that("bara kodkolumner som inte är valda tas bort, även med olika namn i olika tabeller", {
  d <- data.frame(table_id = "A", region_code = "1", region = "x", kön_code = "2", kön = "y",
                  sex_code = "2", sex = "y", kommun_code = "9", value = 1, check.names = FALSE)
  tabs <- list(
    list(variables = data.frame(code = c("Region", "Kon"), label = c("region", "kön"))),
    list(variables = data.frame(code = c("Region", "Kon"), label = c("region", "sex")))
  )
  expect_named(px_keep_code_columns(d, tabs, "Region"),
               c("table_id", "region_code", "region", "kön", "sex", "kommun_code", "value"))
  expect_named(px_keep_code_columns(d, tabs, "Kon"),
               c("table_id", "region", "kön_code", "kön", "sex_code", "sex", "kommun_code", "value"))
})

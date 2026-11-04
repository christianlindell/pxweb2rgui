# Fliken "4. Kommentarer" och knappen "Kommentarer" i flik 1, med testbackenden

kommentar_options <- function() {
  options(pxweb2rgui.backend = mock_backend(), pxweb2rgui.cache_file = tempfile(fileext = ".rds"))
}

# knappens html, som testServer ger den; disabled är ett attribut utan värde
knapp_inaktiv <- function(html) grepl("<button[^>]*\\sdisabled", as.character(html))

test_that("knappen står i flik 1, fliken 4. Kommentarer finns och kryssrutan är borta", {
  html <- as.character(px_app_ui())
  expect_no_match(html, "visa_kommentarer", fixed = TRUE)
  expect_no_match(html, "Visa kommentarer", fixed = TRUE)
  expect_match(html, "4. Kommentarer", fixed = TRUE)
  # fliken har ett eget rullningsområde, eftersom sidan själv inte rullar
  expect_match(html, '<div class="px-kommentarer-sida">', fixed = TRUE)
  # knappen står i rubriken på rutan med trädet i flik 1, före flik 2
  expect_lt(regexpr('id="kommentarer_knapp"', html, fixed = TRUE), regexpr('id="trad"', html, fixed = TRUE)[1])
  expect_gt(regexpr('id="kommentarer_knapp"', html, fixed = TRUE), regexpr('Ämnesområden, underområden', html, fixed = TRUE)[1])
  expect_lt(regexpr('id="trad"', html, fixed = TRUE), regexpr('id="variabel_rubrik"', html, fixed = TRUE)[1])
})

test_that("knappen är inaktiv utan vald tabell och aktiv med en eller flera", {
  old <- kommentar_options()
  on.exit(options(old))
  testServer(px_app_server(), {
    session$setInputs(sprak = "sv")
    expect_match(as.character(output$kommentarer_knapp$html), "Kommentarer", fixed = TRUE)
    expect_true(knapp_inaktiv(output$kommentarer_knapp$html))
    session$setInputs(vald_tabell = "TAB6104")
    expect_false(knapp_inaktiv(output$kommentarer_knapp$html))
    session$setInputs(vald_tabell = c("TAB6104", "TAB5444"))
    expect_false(knapp_inaktiv(output$kommentarer_knapp$html))
    session$setInputs(vald_tabell = "")
    expect_true(knapp_inaktiv(output$kommentarer_knapp$html))
  })
})

test_that("inget hämtas förrän knappen klickas, och klicket ger innehåll och byter flik", {
  old <- kommentar_options()
  on.exit({options(old); mock_comment_calls$n <- NULL})
  mock_comment_calls$n <- 0
  flik <- logical()
  valt <- character()
  local_mocked_bindings(
    px_kommentar_flik = function(session, aktiv) flik <<- c(flik, aktiv),
    nav_select = function(id, selected, ...) valt <<- c(valt, selected)
  )
  testServer(px_app_server(), {
    session$setInputs(sprak = "sv")
    session$setInputs(vald_tabell = "TAB6104")
    session$flushReact()
    expect_identical(mock_comment_calls$n, 0)
    expect_null(kommentar_innehall())
    # fliken är gråtonad så länge den är tom
    expect_false(flik[length(flik)])
    expect_identical(valt, character())

    session$setInputs(hamta_kommentarer = 1)
    expect_identical(mock_comment_calls$n, 1)
    expect_identical(valt, "kommentarer")
    expect_true(flik[length(flik)])
    html <- as.character(output$kommentarer$html)
    expect_match(html, "Kommentarer till vald tabell", fixed = TRUE)
    expect_match(html, "TAB6104", fixed = TRUE)
    expect_match(html, "Testtabell TAB6104", fixed = TRUE)
  })
})

test_that("innehållet raderas när ingen tabell är vald och när valet ändras", {
  old <- kommentar_options()
  on.exit({options(old); mock_comment_calls$n <- NULL})
  mock_comment_calls$n <- 0
  flik <- logical()
  local_mocked_bindings(px_kommentar_flik = function(session, aktiv) flik <<- c(flik, aktiv))
  testServer(px_app_server(), {
    session$setInputs(sprak = "sv")
    session$setInputs(vald_tabell = "TAB6104")
    session$setInputs(hamta_kommentarer = 1)
    expect_false(is.null(kommentar_innehall()))
    expect_true(flik[length(flik)])

    # samma tabell igen: kommentarerna gäller fortfarande
    session$setInputs(vald_tabell = "TAB6104")
    session$flushReact()
    expect_false(is.null(kommentar_innehall()))

    # tomt val
    session$setInputs(vald_tabell = "")
    session$flushReact()
    expect_null(kommentar_innehall())
    expect_false(flik[length(flik)])
    expect_error(output$kommentarer)

    # ändrat val: rensas, och nästa klick visar de nya tabellerna utan att läsa om den gamla
    session$setInputs(vald_tabell = "TAB6104")
    session$setInputs(hamta_kommentarer = 2)
    expect_true(flik[length(flik)])
    n <- mock_comment_calls$n
    session$setInputs(vald_tabell = "TAB5444")
    session$flushReact()
    expect_null(kommentar_innehall())
    expect_false(flik[length(flik)])
    session$setInputs(hamta_kommentarer = 3)
    expect_match(as.character(output$kommentarer$html), "TAB5444", fixed = TRUE)
    expect_no_match(as.character(output$kommentarer$html), "TAB6104", fixed = TRUE)
    expect_identical(mock_comment_calls$n, n + 1)
  })
})

test_that("innehållet raderas när leverantör eller språk byts", {
  old <- kommentar_options()
  on.exit(options(old))
  testServer(px_app_server(), {
    session$setInputs(sprak = "sv")
    session$setInputs(vald_tabell = "TAB6104")
    session$setInputs(hamta_kommentarer = 1)
    expect_false(is.null(kommentar_innehall()))
    session$setInputs(sprak = "en")
    session$flushReact()
    expect_null(kommentar_innehall())
  })
})

test_that("innehåll och sparade kommentarer raderas när appen avslutas", {
  old <- kommentar_options()
  on.exit(options(old))
  local_mocked_bindings(px_stop_app = function(...) invisible(NULL))
  testServer(px_app_server(), {
    session$setInputs(sprak = "sv")
    session$setInputs(vald_tabell = "TAB6104")
    session$setInputs(hamta_kommentarer = 1)
    expect_gt(length(ls(kommentar_cache)), 0)
    session$close()
    expect_identical(length(ls(kommentar_cache)), 0L)
  })
  # nästa körning börjar tom
  testServer(px_app_server(), {
    expect_null(kommentar_innehall())
    expect_identical(length(ls(kommentar_cache)), 0L)
  })
})

test_that("gränssnittet har rubriken pxweb2rgui och koden laddar pxweb2rgui", {
  expect_match(px_query_code("TAB1", "t", list(Tid = "9999")), "library(pxweb2rgui)", fixed = TRUE)
  html <- as.character(px_app_ui())
  expect_match(html, ">\\s*pxweb2rgui\\s*<")
  expect_no_match(html, "statistikdatabaser med PxWeb API", fixed = TRUE)
})

test_that("en tabell: rubrik, nivåer, ingen märkning och säker text", {
  old <- kommentar_options()
  on.exit({options(old); mock_comment_calls$n <- NULL})
  mock_comment_calls$n <- 0
  testServer(px_app_server(), {
    session$setInputs(sprak = "sv")
    session$setInputs(vald_tabell = "TAB6104")
    session$setInputs(hamta_kommentarer = 1)
    html <- as.character(output$kommentarer$html)
    expect_match(html, "Kommentarer till vald tabell", fixed = TRUE)
    expect_match(html, "TAB6104", fixed = TRUE)
    expect_match(html, "Testtabell TAB6104", fixed = TRUE)
    expect_match(html, "Hela tabellen", fixed = TRUE)
    expect_match(html, "Variabel: region", fixed = TRUE)
    expect_match(html, "Region 1", fixed = TRUE)
    # alla kommentarer ser likadana ut, även de som är obligatoriska
    expect_no_match(html, "Obligatorisk", fixed = TRUE)
    expect_no_match(html, "px-kommentar-obl", fixed = TRUE)
    expect_no_match(html, "badge", fixed = TRUE)
    # text blir aldrig html, bara länkar blir klickbara
    expect_match(html, "Gäller &lt;hela&gt; tabellen.", fixed = TRUE)
    expect_match(html, "&lt;script&gt;alert(1)&lt;/script&gt;", fixed = TRUE)
    expect_no_match(html, "<script>", fixed = TRUE)
    # inga egna radbrytningar i texten, som annars visas som tomma rader
    expect_match(html, '<span class="px-kommentar-text">Gäller', fixed = TRUE)
    expect_match(html, 'tabellen.</span>', fixed = TRUE)
    # rutan växer med texten
    expect_no_match(html, "html-fill-item", fixed = TRUE)
    expect_match(html, '<a href="https://example.org/lista" target="_blank" rel="noopener noreferrer">listan</a>', fixed = TRUE)
  })
})

test_that("flera tabeller får var sin ruta, och en tabell läses bara en gång", {
  old <- kommentar_options()
  on.exit({options(old); mock_comment_calls$n <- NULL})
  mock_comment_calls$n <- 0
  testServer(px_app_server(), {
    session$setInputs(sprak = "sv")
    session$setInputs(vald_tabell = c("TAB6104", "TAB5444", "TAB100", "TAB300"))
    session$setInputs(hamta_kommentarer = 1)
    html <- as.character(output$kommentarer$html)
    expect_match(html, "Kommentarer till valda tabeller", fixed = TRUE)
    expect_identical(lengths(regmatches(html, gregexpr("px-kommentar-tabell", html, fixed = TRUE))), 4L)
    # varje kommentar står i sin tabells ruta
    rutor <- strsplit(html, "px-kommentar-tabell", fixed = TRUE)[[1]][-1]
    expect_match(rutor[1], "TAB6104", fixed = TRUE)
    expect_match(rutor[1], "Om region 1.", fixed = TRUE)
    expect_match(rutor[2], "Inga kommentarer för TAB5444", fixed = TRUE)
    expect_no_match(rutor[2], "Om region 1.", fixed = TRUE)
    # fel och varning gäller bara sin tabell
    expect_match(rutor[3], "Kunde inte hämta kommentarerna till TAB100: Servern svarade inte.", fixed = TRUE)
    expect_match(rutor[4], "Inga kommentarer för TAB300", fixed = TRUE)
    expect_match(rutor[4], "could not be read", fixed = TRUE)
    n <- mock_comment_calls$n
    expect_identical(n, 4)

    # sparade tabeller hämtas inte igen; tabellen med fel provas på nytt
    session$setInputs(vald_tabell = c("TAB100", "TAB6104"))
    session$setInputs(hamta_kommentarer = 2)
    html <- as.character(output$kommentarer$html)
    expect_identical(lengths(regmatches(html, gregexpr("px-kommentar-tabell", html, fixed = TRUE))), 2L)
    expect_identical(mock_comment_calls$n, n + 1)
    # ordningen följer valet
    expect_lt(regexpr("TAB100", html, fixed = TRUE), regexpr("TAB6104", html, fixed = TRUE)[1])
  })
})

test_that("länkar i html och markdown blir klickbara bara om de går till http(s)", {
  f <- function(x) as.character(pxweb2rgui:::px_comment_html(x))
  expect_identical(f("<A HREF='https://a.fi/b' TARGET=_blank>Dok</A>"),
                   '<a href="https://a.fi/b" target="_blank" rel="noopener noreferrer">Dok</a>')
  expect_match(f("[Se](https://a.no/x)."), '<a href="https://a.no/x"', fixed = TRUE)
  expect_no_match(f("<a href='javascript:alert(1)'>x</a>"), "<a", fixed = TRUE)
  expect_no_match(f("[x](javascript:alert(1))"), "<a", fixed = TRUE)
  expect_no_match(f("<a href='https://a.fi' onclick='x()'>y</a>"), "onclick='x()'>", fixed = TRUE)
  expect_identical(f("a < b & c"), "a &lt; b &amp; c")
})

test_that("en egen backend utan get_comments ger inga kommentarer", {
  b <- mock_backend()
  b$get_comments <- NULL
  old <- options(pxweb2rgui.backend = b)
  on.exit(options(old))
  expect_identical(nrow(px_backend()$get_comments("x", tab = "T")), 0L)
})

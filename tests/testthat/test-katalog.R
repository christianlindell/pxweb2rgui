test_that("sökvägar läses oavsett hur jsonlite förenklat dem", {
  df <- data.frame(id = c("BE", "BE01"), label = c("Befolkning", "Statistik"))
  as_list <- list(list(id = "BE", label = "Befolkning"), list(id = "BE01", label = "Statistik"))
  for (p in list(df, list(df), as_list, list(as_list))) {
    out <- px_normalise_paths(p)
    expect_length(out, 1)
    expect_equal(out[[1]]$id, c("BE", "BE01"))
  }
  expect_length(px_normalise_paths(NULL), 0)
  expect_length(px_normalise_paths(list()), 0)
})

test_that("trädet byggs från testkatalogen", {
  k <- px_parse_catalogue(mock_backend()$search_tables())
  tr <- px_build_tree(k$paths, px_filter_tables(k$tables)$id)
  labels <- vapply(tr$children, `[[`, "", "label")
  expect_equal(labels, c("Befolkning", "Regionala indelningar", "Arbetsmarknad",
                         "(Utan ämnesområde)"))
  folk <- tr$children[[1]]$children[[1]]$children[[1]]
  expect_equal(folk$label, "Folkmängd")
  # API-ordningen behålls och avslutade tabeller är borta
  expect_equal(folk$tables, c("TAB6104", "TAB5444", "TAB638", "TAB1260"))
  # en tabell med två sökvägar finns på båda ställena men räknas en gång
  expect_equal(tr$children[[2]]$children[[1]]$tables, "TAB6104")
  expect_equal(tr$n, 9)
  expect_equal(px_tree_stats(tr)$subjects, 4)
})

test_that("avslutade tabeller och fritextsökning filtrerar", {
  k <- px_parse_catalogue(mock_backend()$search_tables())
  expect_true("TAB101" %in% px_filter_tables(k$tables, include_discontinued = TRUE)$id)
  expect_false("TAB101" %in% px_filter_tables(k$tables)$id)
  hit <- px_filter_tables(k$tables, "  ANSTÄLLNINGAR region ")
  expect_equal(hit$id, "TAB4718")
  expect_equal(px_filter_tables(k$tables, "tab6104")$id, "TAB6104")
})

test_that("datumfiltret visar bara tabeller uppdaterade sedan datumet", {
  k <- px_parse_catalogue(mock_backend()$search_tables())
  tabs <- k$tables
  tabs$updated <- NA_character_
  tabs$updated[tabs$id == "TAB6104"] <- "2026-11-01"
  tabs$updated[tabs$id == "TAB5444"] <- "2026-10-15"
  tabs$updated[tabs$id == "TAB638"] <- "2026-09-30"
  alla <- nrow(px_filter_tables(tabs))
  expect_equal(px_filter_tables(tabs, updated_since = as.Date("2026-10-15"))$id,
               c("TAB6104", "TAB5444"))
  expect_equal(px_filter_tables(tabs, updated_since = "2026-11-01")$id, "TAB6104")
  # tomt eller ogiltigt datum filtrerar inte
  expect_equal(nrow(px_filter_tables(tabs, updated_since = NULL)), alla)
  expect_equal(nrow(px_filter_tables(tabs, updated_since = as.Date(NA))), alla)
  # kombineras med fritext
  expect_equal(nrow(px_filter_tables(tabs, "anställningar",
                                     updated_since = as.Date("2026-10-15"))), 0)
})

test_that("trädet visar när tabellerna senast uppdaterades", {
  k <- px_parse_catalogue(mock_backend()$search_tables())
  tabs <- k$tables
  tabs$updated <- NA_character_
  tabs$updated[tabs$id == "TAB6104"] <- "2026-04-16"
  html <- as.character(px_tree_html(px_build_tree(k$paths), tabs, open = TRUE))
  expect_match(html, '<span class="px-updated">[2026-04-16]</span>',
               fixed = TRUE)
  # tabeller utan datum får ingen tom ruta
  expect_equal(lengths(regmatches(html, gregexpr("px-updated", html))), 2)  # två sökvägar
})

test_that("trädet blir HTML med klickbara tabeller", {
  k <- px_parse_catalogue(mock_backend()$search_tables())
  html <- as.character(px_tree_html(px_build_tree(k$paths), k$tables, open = TRUE))
  expect_match(html, 'data-id="TAB4707"')
  expect_match(html, "<details class=\"px-node\" data-key=\"BE\" open>")
  expect_match(html, "avslutad")
})

test_that("katalogen cachas på disk", {
  f <- tempfile(fileext = ".rds")
  calls <- 0
  b <- mock_backend()
  st <- b$search_tables
  b$search_tables <- function(...) { calls <<- calls + 1; st(...) }
  px_fetch_catalogue(b, cache_file = f)
  px_fetch_catalogue(b, cache_file = f)
  expect_equal(calls, 1)
  px_fetch_catalogue(b, cache_file = f, force = TRUE)
  expect_equal(calls, 2)
})

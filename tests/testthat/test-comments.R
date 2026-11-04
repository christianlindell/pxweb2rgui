# pxr_get_comments(), utan nätverk

kom_response <- function(url, json, status = 200L) {
  structure(list(url = url, status_code = status,
                 headers = structure(list(`content-type` = "application/json; charset=utf-8"),
                                     class = c("insensitive", "list")),
                 content = charToRaw(enc2utf8(json))),
            class = "response")
}

kom_v2_url <- "https://v2.example/api/v2/tables/"

kom_v2_meta <- '{"version":"2.0","class":"dataset","label":"Folkmängd",
  "note":["Hela tabellen.","Frivillig.\\r\\nAndra raden. "],
  "id":["Region","Tid"],"size":[2,1],
  "dimension":{
    "Region":{"label":"region","note":["Om region."],
      "category":{"index":{"01":0,"02":1},"label":{"01":"Ett","02":"Två"},
                  "note":{"02":["Första om två.","Andra om två."]}},
      "extension":{"noteMandatory":{"0":true},"categoryNoteMandatory":{"02":{"1":true}}}},
    "Tid":{"label":"år","category":{"index":{"2024":0},"label":{"2024":"2024"}}}},
  "extension":{"noteMandatory":{"0":true},"px":{"tableid":"T1"}}}'

kom_v2_tom <- '{"version":"2.0","class":"dataset","label":"Utan","id":["Tid"],"size":[1],
  "dimension":{"Tid":{"label":"år","category":{"index":{"2024":0},"label":{"2024":"2024"}}}},
  "extension":{"px":{"tableid":"T2"}}}'

kom_v2_get <- function(url, ...) {
  json <- if (grepl("/config$", url)) {
    '{"apiVersion":"2.3.2","defaultLanguage":"sv","languages":[{"id":"sv","label":"Svenska"}],
      "maxDataCells":1000,"maxCallsPerTimeWindow":30,"timeWindow":10}'
  } else if (grepl("/T1/metadata$", url)) {
    kom_v2_meta
  } else if (grepl("/T2/metadata$", url)) {
    kom_v2_tom
  }
  if (is.null(json)) return(kom_response(url, "{}", 404L))
  kom_response(url, json)
}

test_that("API v2 gives comments on the table, variables and values, with what must be shown", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  local_mocked_bindings(.pxweb2_GET = kom_v2_get)
  res <- pxr_get_comments(kom_v2_url, "T1")
  expect_s3_class(res, "tbl_df")
  expect_named(res, c("tabellid", "niva", "var_kod", "var_namn", "varde_kod", "varde_namn",
                      "kommentar", "obligatorisk"))
  expect_identical(res$niva, c("tabell", "tabell", "variabel", "värde", "värde"))
  expect_identical(res$tabellid, rep("T1", 5))
  expect_identical(res$kommentar, c("Hela tabellen.", "Frivillig.\nAndra raden.", "Om region.",
                                    "Första om två.", "Andra om två."))
  expect_identical(res$obligatorisk, c(TRUE, FALSE, TRUE, FALSE, TRUE))
  expect_identical(res$var_kod, c(NA, NA, "Region", "Region", "Region"))
  expect_identical(res$var_namn, c(NA, NA, "region", "region", "region"))
  expect_identical(res$varde_kod, c(NA, NA, NA, "02", "02"))
  expect_identical(res$varde_namn, c(NA, NA, NA, "Två", "Två"))
})

test_that("a note given as false in noteMandatory is not mandatory", {
  expect_identical(.pxr_note_mandatory(list("a", "b", "c"), list(`0` = TRUE, `1` = FALSE), FALSE),
                   c(TRUE, FALSE, FALSE))
  expect_identical(.pxr_note_mandatory(list("a"), list(`0` = FALSE), FALSE), FALSE)
  expect_identical(.pxr_note_mandatory(list("a"), NULL, NA), NA)
})

test_that("a table without comments gives an empty result with the same columns, and a message", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  local_mocked_bindings(.pxweb2_GET = kom_v2_get)
  expect_message(res <- pxr_get_comments(kom_v2_url, "T2"), "No comments in: T2")
  expect_identical(nrow(res), 0L)
  expect_named(res, c("tabellid", "niva", "var_kod", "var_namn", "varde_kod", "varde_namn",
                      "kommentar", "obligatorisk"))
  expect_type(res$obligatorisk, "logical")
})

test_that("several tables are combined, and repeated ids are read once", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  local_mocked_bindings(.pxweb2_GET = kom_v2_get)
  expect_message(res <- pxr_get_comments(kom_v2_url, c("T2", "T1", "T1")), "No comments in: T2")
  expect_identical(unique(res$tabellid), "T1")
  expect_identical(nrow(res), 5L)
})

test_that("tab must be table ids", {
  expect_error(pxr_get_comments("scb"), "tab must be")
  expect_error(pxr_get_comments("scb", 638), "tab must be")
  expect_error(pxr_get_comments("scb", NA_character_), "tab must be")
})

kom_v1_url <- "https://v1.example/api/v1/sv/db/"

kom_v1_table <- '{"title":"T","variables":[
  {"code":"Ind","text":"indikator","values":["A","B","C"],"valueTexts":["Aa","Bb","Cc"]},
  {"code":"Tid","text":"år","values":["2023","2024"],"valueTexts":["2023","2024"],"time":true}]}'

kom_v1_get <- function(url, ...) {
  json <- if (grepl("?config", url, fixed = TRUE)) {
    '{"maxValues":100,"maxCells":2,"maxCalls":30,"timeWindow":10}'
  } else if (grepl("?query=", url, fixed = TRUE)) {
    NULL
  } else if (grepl("/db/$", url)) {
    '[{"id":"M","type":"l","text":"M"}]'
  } else if (grepl("/db/M/$", url)) {
    '[{"id":"T.px","type":"t","text":"T"}]'
  } else if (grepl("/db/M/T\\.px$", url)) {
    kom_v1_table
  }
  if (is.null(json)) return(kom_response(url, "{}", 400L))
  kom_response(url, paste0("﻿", json))
}

# Svaren har kommentarer bara för de värden som frågan gäller, som hos riktiga webbplatser.
kom_v1_post <- function(seen = new.env(), status = 200L) {
  function(url, ..., body) {
    seen$url <- url
    b <- jsonlite::fromJSON(as.character(body), simplifyVector = FALSE)
    fmt <- b$response$format
    seen$formats <- c(seen$formats, fmt)
    vals <- unlist(b$query[[1]]$selection$values)
    seen$ind <- c(seen$ind, list(vals))
    if (status != 200L) return(kom_response(url, "fel", status))
    # samma tabellnot i px-formatet ger ingen dubblett; webbplatser utan px provas i ett eget test
    if (fmt == "px") return(kom_response(url, 'NOTE="Om tabellen i px.";\r\nDATA=\r\n1;\r\n'))
    json <- if (fmt == "json-stat2") {
      '{"version":"2.0","class":"dataset","note":["Om tabellen."],"id":["Ind","Tid"],"size":[1,1],
        "dimension":{"Ind":{"label":"indikator","note":["Om indikator."],
          "category":{"index":{"A":0},"label":{"A":"Aa"}}},
          "Tid":{"label":"år","category":{"index":{"2023":0},"label":{"2023":"2023"}}}},
        "value":[1]}'
    } else {
      comments <- if ("C" %in% vals) '{"variable":"Ind","value":"C","comment":"Om C.\\r\\n"}' else ""
      paste0('{"columns":[{"code":"Ind","text":"indikator","type":"d","comment":"Om indikator."},
        {"code":"Tid","text":"år","type":"t"},{"code":"Innehåll","text":"värde","type":"c"}],
        "comments":[', comments, '],"data":[],"metadata":[]}')
    }
    kom_response(url, paste0("﻿", json))
  }
}

test_that("API v1 asks for every value, and the same comment is kept once", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  seen <- new.env()
  local_mocked_bindings(.pxweb2_GET = kom_v1_get, .pxweb2_POST = kom_v1_post(seen))
  res <- suppressMessages(pxr_get_comments(kom_v1_url, "T"))
  # hela sökvägen, fast id:t saknar mappar
  expect_identical(seen$url, "https://v1.example/api/v1/sv/db/M/T.px")
  expect_identical(unique(res$tabellid), "M/T.px")
  expect_identical(res$niva, c("tabell", "variabel", "värde"))
  expect_identical(res$kommentar, c("Om tabellen.", "Om indikator.", "Om C."))
  expect_identical(res$varde_kod, c(NA, NA, "C"))
  expect_identical(res$varde_namn, c(NA, NA, "Cc"))
  expect_identical(res$var_namn, c(NA, "indikator", "indikator"))
  # API:et säger inget om vad som är obligatoriskt
  expect_identical(res$obligatorisk, c(NA, NA, NA))
  # högst maxCells värden per fråga: först json-stat2 och px, sedan Ind i två delar, och Tid
  expect_identical(seen$formats, c("json-stat2", "px", "json", "json", "json"))
  expect_identical(seen$ind, list("A", "A", c("A", "B"), "C", "A"))
})

# En webbplats vars kommentarer på tabell och variabel bara finns i px-formatet
kom_px_post <- function(px = NULL) {
  function(url, ..., body) {
    b <- jsonlite::fromJSON(as.character(body), simplifyVector = FALSE)
    if (b$response$format == "px") {
      if (is.null(px)) return(kom_response(url, "fel", 400L))
      r <- kom_response(url, "")
      r$content <- px
      return(r)
    }
    kom_response(url, if (b$response$format == "json-stat2") {
      '{"version":"2.0","class":"dataset","id":["Ind","Tid"],"size":[1,1],
        "dimension":{"Ind":{"label":"indikator","category":{"index":{"A":0},"label":{"A":"Aa"}}},
          "Tid":{"label":"år","category":{"index":{"2023":0},"label":{"2023":"2023"}}}},"value":[1]}'
    } else {
      '{"columns":[],"comments":[],"data":[],"metadata":[]}'
    })
  }
}

test_that("comments on the table and variables that only format px has are read", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  # iso-8859-15, som i filens CODEPAGE; texten går över flera rader, och ## delar noter
  px <- iconv(paste0('CHARSET="ANSI";\r\nCODEPAGE="iso-8859-15";\r\nNOTE="Första om Curaçao, som är ',
                     'lång och fortsät"\r\n"ter.##Andra:#med radbrytning.";\r\n',
                     'NOTEX="Måste visas.";\r\nNOTE("indikator")="Om indikator.";\r\n',
                     'NOTE("finns inte")="Om okänd variabel.";\r\n',
                     'VALUENOTE("indikator","Aa")="Hämtas med json.";\r\nDATA=\r\n1;\r\n'),
              "UTF-8", "ISO-8859-15", toRaw = TRUE)[[1]]
  local_mocked_bindings(.pxweb2_GET = kom_v1_get, .pxweb2_POST = kom_px_post(px))
  res <- suppressMessages(pxr_get_comments(kom_v1_url, "T"))
  expect_identical(res$kommentar, c("Första om Curaçao, som är lång och fortsätter.",
                                    "Andra:\nmed radbrytning.", "Måste visas.", "Om indikator."))
  expect_identical(res$niva, c("tabell", "tabell", "tabell", "variabel"))
  expect_identical(res$var_kod, c(NA, NA, NA, "Ind"))
  # NOTEX måste visas
  expect_identical(res$obligatorisk, c(FALSE, FALSE, TRUE, FALSE))
})

test_that("a px file in UTF-8 is read as UTF-8 whatever its header says", {
  txt <- enc2utf8('CODEPAGE="iso-8859-15";\r\nNOTE="Curaçao";\r\nDATA=\r\n1;\r\n')
  expect_identical(.pxweb1_px_notes(.pxweb1_px_text(charToRaw(txt)))$kommentar, enc2utf8("Curaçao"))
  bom <- c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw(txt))
  expect_identical(.pxweb1_px_notes(.pxweb1_px_text(bom))$kommentar, enc2utf8("Curaçao"))
})

test_that("a site without format px gives what json has, without a warning", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  local_mocked_bindings(.pxweb2_GET = kom_v1_get, .pxweb2_POST = kom_px_post())
  expect_no_warning(res <- suppressMessages(pxr_get_comments(kom_v1_url, "T")))
  expect_identical(nrow(res), 0L)
})

test_that("an answer that cannot be read in API v1 gives a warning, not an error", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  local_mocked_bindings(.pxweb2_GET = kom_v1_get, .pxweb2_POST = kom_v1_post(status = 403L))
  expect_warning(res <- suppressMessages(pxr_get_comments(kom_v1_url, "M/T.px")),
                 "could not be read \\(HTTP 403\\)")
  expect_identical(nrow(res), 0L)
})

kom_dst_get <- function(url, ...) {
  id <- sub("^.*tableinfo/", "", url)
  fot <- switch(id,
    MED = '{"text":"Måste visas.","mandatory":true}',
    FRI = '{"text":"Får visas.","mandatory":false}',
    '{}')
  kom_response(url, paste0('{"id":"', id, '","text":"T","updated":"2026-01-01T08:00:00","footnote":', fot, ',
    "variables":[{"id":"Tid","text":"tid","time":true,"values":[{"id":"2024","text":"2024"}]}]}'))
}

test_that("Statistics Denmark gives the footnote of the table", {
  pxweb2_clear_cache()
  on.exit(pxweb2_clear_cache())
  local_mocked_bindings(.pxweb2_GET = kom_dst_get)
  res <- pxr_get_comments("dst", c("MED", "FRI"), lang = "en")
  expect_identical(res$tabellid, c("MED", "FRI"))
  expect_identical(res$niva, c("tabell", "tabell"))
  expect_identical(res$kommentar, c("Måste visas.", "Får visas."))
  expect_identical(res$obligatorisk, c(TRUE, FALSE))
  expect_message(tom <- pxr_get_comments("dst", "UTAN", lang = "en"), "No comments in: UTAN")
  expect_identical(nrow(tom), 0L)
})

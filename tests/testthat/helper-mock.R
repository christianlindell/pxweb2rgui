# Testbackend med påhittade SCB-liknande tabeller, för att köra appen och
# testerna utan nätverk. Svaren har samma form som PxWeb API v2 och går
# genom jsonlite precis som i pxweb2r.
#
#   source("tests/mock_backend.R"); options(pxweb2rgui.backend = mock_backend())

mock_tables_json <- function() {
  p <- function(...) {
    x <- list(...)
    lapply(seq(1, length(x), 2), function(i) list(id = x[[i]], label = x[[i + 1]]))
  }
  befolkning <- p("BE", "Befolkning", "BE0101", "Befolkningsstatistik",
                  "BE0101A", "Folkmängd")
  flyttningar <- p("BE", "Befolkning", "BE0101", "Befolkningsstatistik",
                   "BE0101J", "Flyttningar")
  prognos <- p("BE", "Befolkning", "BE0401", "Befolkningsframskrivningar")
  anst <- p("AM", "Arbetsmarknad", "AM005", "Anställningar", "AM005A", "Anställningar")
  syss <- p("AM", "Arbetsmarknad", "AM0207", "Registerbaserad arbetsmarknadsstatistik (RAMS)")
  regional <- p("RE", "Regionala indelningar", "RE01", "Kommuner")

  tab <- function(id, label, paths, first = "2000", last = "2025", disc = FALSE) {
    list(type = "Table", id = id, label = label, updated = "2026-10-01T06:00:00Z",
         firstPeriod = first, lastPeriod = last, category = "public",
         variableNames = list("region", "kön", "tabellinnehåll", "år"),
         discontinued = disc, source = "SCB", subjectCode = if (length(paths)) paths[[1]][[1]]$id else "XX",
         timeUnit = "Annual", paths = paths)
  }
  tables <- list(
    tab("TAB6104", "Folkmängd efter region och kön. År 1968-2025", list(befolkning, regional)),
    tab("TAB5444", "Folkmängden per månad efter region och kön", list(befolkning), "2000M01", "2026M08"),
    tab("TAB638", "Folkmängden efter region, civilstånd, ålder och kön. År 1968-2024", list(befolkning)),
    tab("TAB1260", "Folkmängd efter DeSO-område och kön. År 2010-2024", list(befolkning), "2010", "2024"),
    tab("TAB100", "Flyttningar efter region, ålder och kön. År 1997-2024", list(flyttningar)),
    tab("TAB101", "Folkmängd 1749-1859 (avslutad)", list(befolkning), "1749", "1859", TRUE),
    tab("TAB300", "Framskriven folkmängd efter ålder och kön. År 2025-2070", list(prognos), "2025", "2070"),
    tab("TAB4707", "Antal pågående anställningar efter kön och sektor. Månad", list(anst), "2015M04", "2026M08"),
    tab("TAB4718", "Antal pågående anställningar efter kön, region och sektor. Månad", list(anst), "2020M01", "2026M08"),
    tab("TAB200", "Förvärvsarbetande efter region och kön. År 2004-2018", list(syss), "2004", "2018", TRUE),
    tab("TAB999", "Tabell utan sökväg", list())
  )
  jsonlite::toJSON(list(language = "sv", tables = tables), auto_unbox = TRUE)
}

mock_metadata <- function(id) {
  n_reg <- if (id == "TAB1260") 1500 else 25
  reg_codes <- c("00", sprintf("%02d", 1:(n_reg - 1)))
  reg_labels <- c("Riket", paste("Region", seq_len(n_reg - 1)))
  is_month <- id %in% c("TAB5444", "TAB4707", "TAB4718")
  tid <- if (is_month) sprintf("2025M%02d", 1:12) else as.character(2015:2024)

  cat_list <- function(codes, labels) {
    list(index = as.list(stats::setNames(seq_along(codes) - 1, codes)),
         label = as.list(stats::setNames(labels, codes)))
  }
  dim <- list(
    Region = list(label = "region", category = cat_list(reg_codes, reg_labels),
                  extension = list(
                    elimination = TRUE, show = "code_value",
                    codelists = list(
                      list(id = "vs_RegionLän07", label = "Län", type = "Valueset",
                           links = list(list(rel = "metadata", href = "https://x/codelists/vs_RegionLän07"))),
                      list(id = "agg_RegionA-region", label = "A-regioner", type = "Aggregation",
                           links = list(list(rel = "metadata", href = "https://x/codelists/agg_RegionA-region")))
                    ))),
    Kon = list(label = "kön", category = cat_list(c("1", "2"), c("män", "kvinnor")),
               extension = list(elimination = id != "TAB4718")),
    ContentsCode = list(label = "tabellinnehåll",
                        category = cat_list(c("BE0101N1", "BE0101N2"),
                                            c("Folkmängd", "Folkökning")),
                        extension = list(elimination = FALSE)),
    Tid = list(label = if (is_month) "månad" else "år", category = cat_list(tid, tid),
               extension = list(elimination = FALSE))
  )
  meta <- list(
    version = "2.0", class = "dataset",
    label = paste("Testtabell", id), source = "SCB",
    role = list(time = list("Tid"), geo = list("Region"), metric = list("ContentsCode")),
    id = as.list(names(dim)), size = as.list(vapply(dim, function(d) length(d$category$label), 1L)),
    dimension = dim,
    extension = list(px = list(tableid = id))
  )
  jsonlite::fromJSON(jsonlite::toJSON(meta, auto_unbox = TRUE), simplifyVector = FALSE)
}

mock_backend <- function(pxweb2r_source = Sys.getenv("PXWEB2R_SRC")) {
  # Riktiga get_variables()/get_values() från paketet (eller från PXWEB2R_SRC)
  px_env <- new.env()
  if (nzchar(pxweb2r_source) && file.exists(pxweb2r_source)) {
    sys.source(pxweb2r_source, envir = px_env)
  }
  list(
    providers = function() {
      data.frame(id = c("scb", "ssb"),
                 name = c("SCB", "SSB"),
                 api_url = c("https://scb.example/api/v2/", "https://ssb.example/api/v2/"),
                 tables_url = c("https://scb.example/api/v2/tables/", "https://ssb.example/api/v2/tables/"),
                 default_lang = c("sv", "no"), languages = c("sv,en", "no,en"),
                 stringsAsFactors = FALSE)
    },
    languages = function(base_url) {
      ids <- if (grepl("ssb", base_url)) c("no", "en") else c("sv", "en")
      out <- data.frame(id = ids, label = ids, stringsAsFactors = FALSE)
      attr(out, "default") <- ids[1]
      out
    },
    max_cells = function(base_url) if (grepl("ssb", base_url)) 800000 else 150000,
    search_tables = function(query = NULL, ...) {
      tibble::as_tibble(jsonlite::fromJSON(mock_tables_json(), simplifyVector = TRUE)$tables)
    },
    get_metadata = function(table_id, ...) {
      if (table_id == "TAB999") stop("Table id 'TAB999' was not found (HTTP 404).")
      mock_metadata(table_id)
    },
    get_variables = function(meta, ...) {
      if (exists("pxweb2_get_variables", px_env)) return(get("pxweb2_get_variables", px_env)(meta))
      data.frame(code = names(meta$dimension),
                 label = vapply(meta$dimension, `[[`, "", "label"),
                 size = vapply(meta$dimension, function(d) length(d$category$label), 1L),
                 elimination = vapply(meta$dimension, function(d) isTRUE(d$extension$elimination), TRUE),
                 role = c("geo", "other", "contents", "time"))
    },
    get_values = function(meta, ...) {
      if (exists("pxweb2_get_values", px_env)) return(get("pxweb2_get_values", px_env)(meta, ...))
      lapply(meta$dimension, function(d) {
        data.frame(code = names(d$category$label), label = unlist(d$category$label),
                   type = "Variable")
      })
    },
    list_codelists = function(meta, ...) {
      if (exists("pxweb2_list_codelists", px_env)) return(get("pxweb2_list_codelists", px_env)(meta))
      data.frame(variable = "Region", id = c("vs_RegionLän07", "agg_RegionA-region"),
                 type = c("Valueset", "Aggregation"), label = c("Län", "A-regioner"))
    },
    get_codelist = function(codelist_id, ...) {
      if (codelist_id == "vs_RegionLän07") {
        return(data.frame(code = sprintf("%02d", 1:5), label = paste("Län", 1:5)))
      }
      data.frame(code = c("A1", "A2"), label = c("Storstäder", "Övriga"))
    },
    get_data = function(table, query, ..., code_variables = NULL) {
      meta <- table
      vals <- lapply(names(meta$dimension), function(v) {
        q <- query[[v]]
        all <- names(meta$dimension[[v]]$category$label)
        if (is.null(q) || identical(q, "*")) return(all)
        if (length(q) == 1 && is.na(q)) return(NULL)
        if (identical(q, "9999")) return(utils::tail(all, 1))
        if (grepl("^top\\(", q[1])) return(utils::tail(all, as.integer(gsub("\\D", "", q[1]))))
        q
      })
      names(vals) <- vapply(meta$dimension, `[[`, "", "label")
      d <- expand.grid(Filter(Negate(is.null), vals), stringsAsFactors = FALSE)
      # som pxweb2r: kodkolumn <variabel>_code före klartextkolumnen
      # code_variables kan vara koder (t.ex. "Region") eller namn
      kod_lab <- vapply(code_variables, function(cv) {
        if (cv %in% names(meta$dimension)) meta$dimension[[cv]]$label else cv
      }, "", USE.NAMES = FALSE)
      for (lab in rev(intersect(kod_lab, names(d)))) {
        pos <- match(lab, names(d))
        d <- cbind(d[seq_len(pos - 1)], stats::setNames(list(d[[lab]]), paste0(lab, "_code")),
                   d[pos:ncol(d)])
      }
      d$varde <- round(stats::runif(nrow(d), 100, 10000))
      tibble::as_tibble(d)
    },
    # TAB6104 har kommentarer, TAB100 ger fel, TAB300 en varning; övriga har inga
    get_comments = function(provider, tab, lang = NULL) {
      if (!is.null(mock_comment_calls$n)) mock_comment_calls$n <- mock_comment_calls$n + 1
      if (tab == "TAB100") stop("Servern svarade inte.")
      if (tab == "TAB300") warning("Some comments could not be read (HTTP 403).")
      if (tab != "TAB6104") {
        message("No comments in: ", tab, ".")
        return(pxweb2rgui:::.pxr_no_comments())
      }
      tibble::tibble(
        tabellid = tab,
        niva = c("tabell", "variabel", "värde"),
        var_kod = c(NA, "Region", "Region"),
        var_namn = c(NA, "region", "region"),
        varde_kod = c(NA, NA, "01"),
        varde_namn = c(NA, NA, "Region 1"),
        kommentar = c("Gäller <hela> tabellen.", "Om region, se [listan](https://example.org/lista).",
                      "<script>alert(1)</script>Om region 1."),
        obligatorisk = c(TRUE, FALSE, NA)
      )
    }
  )
}

# räknar anropen när testet sätter mock_comment_calls$n <- 0
mock_comment_calls <- new.env()

# Backend: de pxweb2r-funktioner appen använder.
#
# Appen anropar aldrig pxweb2r direkt utan går via px_backend(). Det gör att
# man kan byta ut anropen (t.ex. mot testdata) med
#   options(pxweb2rgui.backend = list(search_tables = ..., ...))
#
# Alla anrop får base_url (statistikleverantörens webbplats) och lang
# (språket). Argument som den installerade pxweb2r inte har tas bort, så att
# appen även fungerar med en äldre pxweb2r (då med SCB och svenska/engelska).

px_default_backend <- function() {
  list(
    search_tables  = function(...) px_call_pxweb2r("pxweb2_search_tables", ...),
    get_metadata   = function(...) px_call_pxweb2r("pxweb2_get_metadata", ...),
    get_variables  = function(...) px_call_pxweb2r("pxweb2_get_variables", ...),
    get_values     = function(...) px_call_pxweb2r("pxweb2_get_values", ...),
    list_codelists = function(...) px_call_pxweb2r("pxweb2_list_codelists", ...),
    get_codelist   = function(...) px_call_pxweb2r("pxweb2_get_codelist", ...),
    get_data       = px_default_backend_get_data,
    get_comments   = function(...) px_call_pxweb2r("pxr_get_comments", ...),
    providers      = px_default_providers,
    languages      = px_default_languages,
    max_cells      = px_default_max_cells
  )
}

# Fyller i det som saknas i en egen backend (t.ex. testbackenden)
px_backend <- function() {
  b <- getOption("pxweb2rgui.backend")
  if (is.null(b)) return(px_default_backend())
  if (is.null(b$providers)) b$providers <- px_fallback_providers
  if (is.null(b$languages)) b$languages <- function(base_url) px_fallback_languages(base_url)
  if (is.null(b$max_cells)) b$max_cells <- function(base_url) 150000
  if (is.null(b$get_comments)) b$get_comments <- function(...) .pxr_no_comments()
  b
}


# Anropar <name>() med de argument funktionen har
# Paketets egen funktion med namnet name
px_fun <- function(name) get(name, envir = environment(px_fun), mode = "function")

px_call_pxweb2r <- function(name, ...) {
  fun <- px_fun(name)
  args <- list(...)
  f <- names(formals(fun))
  if (!"..." %in% f && !is.null(names(args))) {
    args <- args[!nzchar(names(args)) | names(args) %in% f]
  }
  do.call(fun, args)
}

# Kända leverantörer om pxweb2r inte har pxweb2_providers()
px_fallback_providers <- function() {
  data.frame(
    id = "scb",
    name = "Statistiska centralbyrån (SCB), Sverige",
    api_url = "https://statistikdatabasen.scb.se/api/v2/",
    tables_url = "https://statistikdatabasen.scb.se/api/v2/tables/",
    default_lang = "sv",
    languages = "sv,en",
    stringsAsFactors = FALSE
  )
}

px_default_providers <- function() {
  if (px_pxweb2r_has("pxweb2_providers")) return(pxweb2_providers())
  px_fallback_providers()
}

px_fallback_languages <- function(base_url) {
  prov <- tryCatch(px_backend()$providers(), error = function(e) px_fallback_providers())
  hit <- match(base_url, prov$tables_url)
  ids <- if (is.na(hit)) c("sv", "en") else trimws(strsplit(prov$languages[hit], ",")[[1]])
  ids <- ids[!is.na(ids) & nzchar(ids) & ids != "NA"]
  if (length(ids) == 0) ids <- c("sv", "en")
  out <- data.frame(id = ids, label = ids, stringsAsFactors = FALSE)
  d <- if (is.na(hit)) NA_character_ else as.character(prov$default_lang[hit])
  attr(out, "default") <- if (length(d) == 1 && !is.na(d) && d %in% ids) d else ids[1]
  out
}

px_default_languages <- function(base_url) {
  if (px_pxweb2r_has("pxweb2_languages")) {
    res <- tryCatch(pxweb2_languages(base_url), error = function(e) NULL)
    if (!is.null(res) && nrow(res) > 0) return(res)
  }
  px_fallback_languages(base_url)
}

# Största antal celler per anrop enligt API:ets /config
px_default_max_cells <- function(base_url) {
  if (!px_pxweb2r_has("pxweb2_get_config")) return(150000)
  cfg <- tryCatch(pxweb2_get_config(base_url), error = function(e) NULL)
  as.numeric(cfg$maxDataCells %||% 150000)
}

px_pxweb2r_has <- function(name) {
  exists(name, envir = environment(px_pxweb2r_has), mode = "function")
}

# Språknamn att visa i listan. API:et ger namnen på sitt eget språk; för
# vanliga koder används svenska namn.
px_language_names <- c(sv = "svenska", en = "engelska", no = "norska", nb = "norska (bokmål)",
                       nn = "nynorska", fi = "finska", da = "danska", is = "isländska",
                       de = "tyska", fr = "franska", et = "estniska", lv = "lettiska",
                       lt = "litauiska")

px_language_choices <- function(langs) {
  ids <- as.character(langs$id)
  lab <- unname(px_language_names[ids])
  lab[is.na(lab)] <- as.character(langs$label)[is.na(lab)]
  lab[is.na(lab) | !nzchar(lab)] <- ids[is.na(lab) | !nzchar(lab)]
  stats::setNames(ids, paste0(lab, " (", ids, ")"))
}

# Hämtar data. code_variables skickas bara med när användaren valt kodkolumner,
# så att appen fortsätter fungera med en äldre pxweb2r utan argumentet.
px_get_data <- function(backend, ..., code_variables = NULL) {
  if (length(code_variables) == 0) return(backend$get_data(...))
  if (identical(backend$get_data, px_default_backend_get_data) &&
      !"code_variables" %in% names(formals(pxr_pxweb2_get_data))) {
    stop("Den installerade versionen av pxweb2r kan inte hämta kodkolumner ",
         "(argumentet code_variables saknas). Installera en nyare pxweb2r.",
         call. = FALSE)
  }
  backend$get_data(..., code_variables = code_variables)
}

px_default_backend_get_data <- function(...) px_call_pxweb2r("pxr_pxweb2_get_data", ...)

# Leverantörerna som data.frame med kolumnerna id, name och tables_url
px_providers_df <- function(backend = px_backend()) {
  prov <- tryCatch(as.data.frame(backend$providers(), stringsAsFactors = FALSE),
                   error = function(e) px_fallback_providers())
  if (nrow(prov) == 0 || !"id" %in% names(prov)) prov <- px_fallback_providers()
  if (!"tables_url" %in% names(prov)) prov$tables_url <- px_tables_url(prov$api_url)
  if (!"name" %in% names(prov)) prov$name <- prov$id
  prov$name[is.na(prov$name)] <- prov$id[is.na(prov$name)]
  prov
}

# Valen i listan "Statistikleverantör", plus en egen adress
px_provider_choices <- function(backend = tryCatch(px_backend(), error = function(e) NULL)) {
  prov <- if (is.null(backend)) px_fallback_providers() else px_providers_df(backend)
  c(stats::setNames(prov$id, prov$name), "Annan webbplats (ange adress)" = "_annan")
}

# Förvald leverantör: options(pxweb2rgui.provider = "ssb"), annars SCB
px_start_provider <- function() {
  getOption("pxweb2rgui.provider", "scb")
}

# "…/api/v2", "…/api/v2/" eller "…/api/v2/tables" -> "…/api/v2/tables/"
# En adress till API v1 ("…/v1/sv/db") har ingen tables-del.
px_tables_url <- function(url) {
  if (.dst_is_url(url)) return(.dst_base_url)
  if (.pxweb1_is_url(url)) return(.pxweb1_normalise_url(url))
  url <- sub("/+$", "", trimws(url))
  url <- sub("/tables$", "", url)
  paste0(url, "/tables/")
}

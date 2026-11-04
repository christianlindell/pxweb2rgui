# Statistics providers (sites) that publish data through PxWeb API v1 or v2,
# and language handling.
#
# Every function that talks to the API takes `base_url` (the tables endpoint,
# e.g. "https://statistikdatabasen.scb.se/api/v2/tables/") and `lang`.
# `base_url` may also be given as a provider id ("scb", "ssb") or as the API
# root ("https://data.ssb.no/api/pxwebapi/v2"); it is normalised to the tables
# endpoint. `lang = NULL` means getOption("pxweb2r.lang") or, failing that,
# the provider's default language read from the API's /config.
#
# A URL with /v1/ (or /v0/) in it is PxWeb API v1, see pxweb1.R. Its base_url
# has no tables endpoint: it is the language and database level,
# e.g. "https://statistik.konj.se/PxWeb/api/v1/sv/KonjBar/". A provider that also
# has API v2 is always used through v2, see .pxweb1_latest_url().
#
# Statistics Denmark ("dst") has an API of its own, see dst.R.

.pxweb2_builtin_providers <- function() {
  data.frame(
    id = c("scb", "ssb", "statfin", "fohm", "sjv", "tillvaxtanalys", "konj", "skolverket",
           "vgr_arbetsmarknad", "vgr_demografi", "vgr_folkhalsa", "vgr_gransregional", "vgr_personal",
           "dst", "nordicstats"),
    name = c("Statistiska centralbyr\u00e5n (SCB), Sverige",
             "Statistisk sentralbyr\u00e5 (SSB), Norge",
             "Statistikcentralen (StatFin), Finland",
             "Folkh\u00e4lsomyndigheten, Sverige",
             "Jordbruksverket, Sverige",
             "Tillv\u00e4xtanalys, Sverige",
             "Konjunkturinstitutet, Sverige",
             "Skolverket, Sverige",
             # one provider per database, as for the other sites with API v1
             "V\u00e4stra G\u00f6talandsregionen: Arbetsmarknad och n\u00e4ringsliv",
             "V\u00e4stra G\u00f6talandsregionen: Demografi",
             "V\u00e4stra G\u00f6talandsregionen: Folkh\u00e4lsoenk\u00e4ten",
             "V\u00e4stra G\u00f6talandsregionen: Gr\u00e4nsregional statistik Norge-Sverige",
             "V\u00e4stra G\u00f6talandsregionen: Personalstatistik",
             "Danmarks Statistik, Danmark",
             "Nordic Statistics (Nordregio), Norden"),
    api_url = c("https://statistikdatabasen.scb.se/api/v2/",
                "https://data.ssb.no/api/pxwebapi/v2/",
                "https://pxdata.stat.fi/PXWeb/api/v1/sv/StatFin/",
                "https://fohm-app.folkhalsomyndigheten.se/Folkhalsodata/api/v1/sv/A_Folkhalsodata/",
                "https://statistik.sjv.se/PXWeb/api/v1/sv/Jordbruksverkets statistikdatabas/",
                "https://statistik.tillvaxtanalys.se/PxWeb/api/v1/sv/Tillv\u00e4xtanalys statistikdatabas/",
                "https://statistik.konj.se/PxWeb/api/v1/sv/KonjBar/",
                "https://statistikdatabasen.skolverket.se/PxWeb/api/v1/sv/Skolverkets_statistikdatabas/",
                "https://pxweb2022.vgregion.se/Pxwebb/api/v1/sv/Arbetsmarknad och N\u00e4ringsliv/",
                "https://pxweb2022.vgregion.se/Pxwebb/api/v1/sv/Demografi/",
                "https://pxweb2022.vgregion.se/Pxwebb/api/v1/sv/Folkh\u00e4lsoenk\u00e4ten (H\u00e4lsa p\u00e5 lika villkor)/",
                "https://pxweb2022.vgregion.se/Pxwebb/api/v1/sv/Gr\u00e4nsregional statistik Norge-Sverige/",
                "https://pxweb2022.vgregion.se/Pxwebb/api/v1/sv/Personalstatistik V\u00e4stra G\u00f6talandsregionen/",
                .dst_base_url,
                "https://pxweb.nordicstatistics.org/api/v1/en/Nordic Statistics/"),
    default_lang = c("sv", "no", "sv", "sv", "sv", "sv", "sv", "sv", rep("sv", 5), "da", "en"),
    languages = c("sv,en", "no,en", "fi,sv,en", "sv,en", "sv", "sv", "sv,en", "sv", rep("sv", 5), "da,en", "en"),
    stringsAsFactors = FALSE
  )
}

#' PxWeb API providers
#'
#' Lists the statistics providers (sites) the package knows of. Statistics
#' Sweden (SCB), Statistics Norway (SSB), Statistics Denmark (with an API of its
#' own) and sites with only API v1 are built
#' in; more can be added for
#' the session with `pxweb2_add_provider()` or permanently with
#' `options(pxweb2r.providers = data.frame(id =, name =, api_url =))`, e.g. in
#' `.Rprofile`. Any PxWeb API site can also be used directly by passing its
#' URL as `base_url`.
#'
#' @return A data frame with `id`, `name`, `api_url` (the API root),
#'   `tables_url` (what `base_url` should be), `default_lang` and `languages`
#'   (comma-separated, as known without asking the API; see
#'   `pxweb2_languages()` for the API's own list).
#' @noRd
pxweb2_providers <- function() {
  out <- .pxweb2_builtin_providers()
  extra <- getOption("pxweb2r.providers")
  if (!is.null(extra)) {
    extra <- as.data.frame(extra, stringsAsFactors = FALSE)
    if (!all(c("id", "api_url") %in% names(extra))) {
      stop("options(pxweb2r.providers) must have the columns id and api_url.", call. = FALSE)
    }
    for (nm in setdiff(names(out), names(extra))) extra[[nm]] <- NA_character_
    extra$name[is.na(extra$name)] <- extra$id[is.na(extra$name)]
    extra <- extra[, names(out)]
    out <- out[!tolower(out$id) %in% tolower(extra$id), , drop = FALSE]
    out <- rbind(out, extra)
  }
  out$api_url <- vapply(out$api_url, .pxweb2_api_root_from_url, "", USE.NAMES = FALSE)
  out$tables_url <- vapply(out$api_url, .pxweb2_tables_url_from_root, "", USE.NAMES = FALSE)
  rownames(out) <- NULL
  out
}

#' Add a PxWeb API provider for this session
#'
#' @param id Short id used as `base_url`, e.g. `"myorg"`. Obligatorisk.
#' @param api_url The API root, e.g. `"https://example.org/api/v2/"` (the
#'   tables endpoint is accepted too), or for API v1 the language and database
#'   level, e.g. `"https://example.org/api/v1/sv/db/"`. Obligatorisk.
#' @param name Display name.
#' @param default_lang Default language code, if known.
#' @param languages Language codes, if known, e.g. `c("sv", "en")`.
#'
#' @return The updated provider list (invisibly).
#' @noRd
pxweb2_add_provider <- function(id, api_url, name = id,
                                default_lang = NA_character_,
                                languages = NA_character_) {
  if (!is.character(id) || length(id) != 1 || !nzchar(id) || grepl("[/:]", id)) {
    stop("id must be a short name without '/' or ':'.", call. = FALSE)
  }
  languages <- as.character(languages)
  languages <- languages[!is.na(languages) & nzchar(languages)]
  row <- data.frame(id = id, name = name, api_url = api_url,
                    default_lang = as.character(default_lang)[1],
                    languages = if (length(languages) == 0) NA_character_ else
                      paste(languages, collapse = ","),
                    stringsAsFactors = FALSE)
  extra <- getOption("pxweb2r.providers")
  if (!is.null(extra)) {
    extra <- as.data.frame(extra, stringsAsFactors = FALSE)
    extra <- extra[!tolower(extra$id) %in% tolower(id), , drop = FALSE]
    for (nm in setdiff(names(row), names(extra))) extra[[nm]] <- NA_character_
    row <- rbind(extra[, names(row)], row)
  }
  options(pxweb2r.providers = row)
  invisible(pxweb2_providers())
}

#' Base URL for a provider
#'
#' @param provider A provider id from `pxweb2_providers()` (e.g. `"scb"`,
#'   `"ssb"`) or a URL to a PxWeb API (API root or tables endpoint).
#'
#' @return The tables endpoint, ending in `"tables/"`, to use as `base_url`.
#'   For API v1 the language and database level, ending in `"/"`.
#' @noRd
pxweb2_base_url <- function(provider = "scb") {
  .pxweb2_normalise_base_url(provider)
}

# Default base_url for every function: getOption("pxweb2r.base_url") (a
# provider id or URL) or Statistics Sweden.
.pxweb2_default_base_url <- function() {
  getOption("pxweb2r.base_url", "https://statistikdatabasen.scb.se/api/v2/tables/")
}

# "…/api/v2", "…/api/v2/", "…/api/v2/tables" -> "…/api/v2/"
.pxweb2_api_root_from_url <- function(url) {
  if (.dst_is_url(url)) return(.dst_base_url)
  if (.pxweb1_is_url(url)) return(.pxweb1_normalise_url(url))
  url <- sub("/+$", "", trimws(url))
  url <- sub("/tables$", "", url)
  paste0(url, "/")
}

.pxweb2_tables_url_from_root <- function(root) {
  if (.pxweb1_is_url(root) || .dst_is_url(root)) root else paste0(root, "tables/")
}

.pxweb2_normalise_base_url <- function(base_url) {
  if (is.null(base_url) || length(base_url) != 1 || is.na(base_url) || !nzchar(trimws(base_url))) {
    base_url <- .pxweb2_default_base_url()
  }
  base_url <- trimws(base_url)
  if (!grepl("^https?://", base_url)) {
    prov <- pxweb2_providers()
    hit <- match(tolower(base_url), tolower(prov$id))
    if (is.na(hit)) {
      stop("Unknown provider '", base_url, "'. Known providers: ",
           paste(prov$id, collapse = ", "),
           ". Give a URL to a PxWeb API or add a provider with options(pxweb2r.providers = ...).",
           call. = FALSE)
    }
    return(.pxweb1_latest_url(prov$tables_url[hit]))
  }
  .pxweb2_tables_url_from_root(.pxweb2_api_root_from_url(.pxweb1_latest_url(base_url)))
}

#' Languages offered by a PxWeb API
#'
#' Reads the languages from the API's `/config`. If that cannot be read, the
#' languages listed in `pxweb2_providers()` are used. API v1 has no list of
#' languages, so it always uses the provider list and the language in the URL.
#'
#' @param base_url Provider id or base URL, see `pxweb2_base_url()`.
#'
#' @return A data frame with `id` and `label`, and the attribute `default`
#'   (the API's default language).
#' @noRd
pxweb2_languages <- function(base_url = .pxweb2_default_base_url()) {
  base_url <- .pxweb2_normalise_base_url(base_url)
  config <- .pxweb2_get_config(base_url)
  .pxweb2_languages_from_config(config, base_url)
}

.pxweb2_languages_from_config <- function(config, base_url) {
  if (.pxweb1_is_url(base_url)) return(.pxweb1_languages(base_url))
  langs <- config$languages
  ids <- character(); labels <- character()
  if (is.list(langs) && length(langs) > 0) {
    ids <- vapply(langs, function(l) as.character(l$id %||% NA_character_), "")
    labels <- vapply(langs, function(l) as.character(l$label %||% l$id %||% NA_character_), "")
  } else if (is.data.frame(langs) && "id" %in% names(langs)) {
    ids <- as.character(langs$id)
    labels <- as.character(langs$label %||% langs$id)
  }
  default <- as.character(config$defaultLanguage %||% NA_character_)[1]

  if (length(ids) == 0) {
    prov <- pxweb2_providers()
    hit <- match(base_url, prov$tables_url)
    if (!is.na(hit)) {
      ids <- strsplit(prov$languages[hit] %||% "", ",")[[1]]
      ids <- trimws(ids)
      ids <- ids[!is.na(ids) & nzchar(ids) & ids != "NA"]
      if (is.na(default) && length(ids) > 0) default <- prov$default_lang[hit]
    }
    labels <- ids
  }
  if (is.na(default) && length(ids) > 0) default <- ids[1]
  out <- data.frame(id = ids, label = labels, stringsAsFactors = FALSE)
  attr(out, "default") <- default
  out
}

# NULL -> getOption("pxweb2r.lang") -> the API's default language -> "sv".
# A language the API does not offer is an error.
.pxweb2_resolve_lang <- function(lang, base_url) {
  if (is.null(lang)) lang <- getOption("pxweb2r.lang")
  langs <- tryCatch(
    .pxweb2_languages_from_config(.pxweb2_get_config(base_url), base_url),
    error = function(e) NULL
  )
  if (is.null(lang)) {
    lang <- attr(langs, "default") %||% NA_character_
    if (is.na(lang)) lang <- "sv"
    return(lang)
  }
  if (!is.character(lang) || length(lang) != 1 || is.na(lang) || !nzchar(lang)) {
    stop("lang must be a single language code, e.g. \"sv\" or \"en\".", call. = FALSE)
  }
  if (!is.null(langs) && nrow(langs) > 0) {
    hit <- match(tolower(lang), tolower(langs$id))
    if (is.na(hit)) {
      # an API v1 site outside the provider list: its languages are not known
      if (identical(attr(langs, "complete"), FALSE)) return(lang)
      stop("Language '", lang, "' is not offered by ", base_url, ". Available: ",
           paste(langs$id, collapse = ", "), ".", call. = FALSE)
    }
    lang <- langs$id[hit]   # the API's own spelling, e.g. "en" for "EN"
  }
  lang
}

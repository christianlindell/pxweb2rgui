# Lists of the tables a provider publishes.

# provider given as id ("scb"), name ("Statistiska centralbyrån (SCB), Sverige",
# case-insensitive) or URL -> the tables endpoint stored in the package
.pxr_tables_url <- function(provider) {
  if (missing(provider) || is.null(provider) || length(provider) != 1 ||
      is.na(provider) || !nzchar(trimws(provider))) {
    stop("provider must be one provider id, provider name or URL", call. = FALSE)
  }
  provider <- trimws(provider)
  if (!grepl("^https?://", provider)) {
    prov <- pxweb2_providers()
    hit <- match(tolower(provider), tolower(prov$name))
    if (!is.na(hit)) provider <- prov$id[hit]
  }
  pxweb2_base_url(provider)
}

# "2026-05-12" -> Date; NULL stays NULL
.pxr_since_date <- function(updated_since) {
  if (is.null(updated_since)) return(NULL)
  d <- if (length(updated_since) == 1 && is.character(updated_since) &&
           grepl("^\\d{4}-\\d{2}-\\d{2}$", updated_since)) {
    as.Date(updated_since, format = "%Y-%m-%d", optional = TRUE)
  } else NA
  if (is.na(d)) {
    stop("updated_since must be a date as text in the format year-month-day, ",
         "e.g. \"2026-05-12\"", call. = FALSE)
  }
  d
}

.pxr_tabs <- function(provider, updated_since, lang) {
  base_url <- .pxr_tables_url(provider)
  since <- .pxr_since_date(updated_since)
  args <- list(base_url = base_url, lang = lang)
  # let the API do a first selection; the exact date is checked below
  if (!is.null(since)) {
    args$pastDays <- max(1, as.integer(Sys.Date() - since) + 1)
  }
  raw <- do.call(pxweb2_search_tables, args)
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  col <- function(name) {
    if (name %in% names(raw)) as.character(raw[[name]]) else rep(NA_character_, nrow(raw))
  }
  out <- data.frame(tabellid = col("id"),
                    tab_name = col("label"),
                    updated = as.Date(substr(col("updated"), 1, 10)),
                    stringsAsFactors = FALSE)
  if (!is.null(since)) out <- out[!is.na(out$updated) & out$updated >= since, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' All tables of a provider
#'
#' Lists the tables that a provider publishes, optionally only those updated
#' on or after a given date.
#'
#' PxWeb API v1 has no list of tables. On sites without search, such as
#' `"skolverket"`, every folder is read, which on a large site can take some
#' minutes the first time in a session; a message says so when it starts. In API
#' v1 `tabellid` is the
#' table's path, which can be used as it is in the other functions.
#'
#' @param provider The statistics provider, by default `"scb"`. It can also be the
#'   provider's full name as the package stores it (e.g. the Swedish name of
#'   Statistics Sweden) or a URL to a PxWeb API v1 or v2; a provider with both
#'   versions is always used through v2. The names are case-insensitive:
#'   * `"scb"`: Statistics Sweden (API v2)
#'   * `"ssb"`: Statistics Norway (API v2)
#'   * `"dst"`: Statistics Denmark (an API of its own)
#'   * `"statfin"`: Statistics Finland (API v1)
#'   * `"fohm"`: Public Health Agency of Sweden (API v1)
#'   * `"sjv"`: Swedish Board of Agriculture (API v1)
#'   * `"tillvaxtanalys"`: Swedish Agency for Growth Policy Analysis (API v1)
#'   * `"konj"`: National Institute of Economic Research, Sweden (API v1)
#'   * `"skolverket"`: Swedish National Agency for Education (API v1)
#'   * `"vgr_arbetsmarknad"`, `"vgr_demografi"`, `"vgr_folkhalsa"`,
#'     `"vgr_gransregional"`, `"vgr_personal"`: the databases of Region Västra
#'     Götaland (API v1)
#'   * `"nordicstats"`: Nordic Statistics database, statistics on all the Nordic
#'     countries from Nordregio (nordicstatistics.org, API v1, English only)
#'   * `"csn"`: Swedish Board of Student Finance (API v1, Swedish only)
#'   * `"energimyndigheten"`: Swedish Energy Agency (API v1)
#'   * `"mcf"`, also `"msb"`: Swedish Civil Defence and Resilience Agency, formerly
#'     MSB (API v1)
#'   * `"skogsstyrelsen"`, `"skogsstyrelsen_d"`: the two databases of the Swedish
#'     Forest Agency (API v1)
#'   * `"slu_skog"`: Swedish National Forest Inventory, SLU (API v1)
#' @param updated_since Keep only tables updated on or after this date, given
#'   as text in the format year-month-day, e.g. `"2026-05-12"`. `NULL` (the
#'   default) keeps all tables.
#' @param lang Language code; `NULL` = `getOption("pxweb2r.lang")` or the
#'   provider's default language.
#'
#' @return A data frame with the columns `tabellid` (table id), `tab_name`
#'   (table name) and `updated` (the date the table was last updated, class
#'   `Date`), one row per table.
#' @seealso [pxr_search_tabs()]
#' @export
#' @example inst/examples/pxr_get_all_tabs.R
pxr_get_all_tabs <- function(provider = "scb", updated_since = NULL, lang = NULL) {
  .pxr_tabs(provider, updated_since, lang)
}

#' Search tables by name
#'
#' Lists the tables of a provider whose names contain any of the given texts,
#' optionally only those updated on or after a given date. The matching is
#' literal (not regular expressions) and does not distinguish between upper
#' and lower case. In PxWeb API v1 the whole table list is read first, see
#' [pxr_get_all_tabs()].
#'
#' @inheritParams pxr_get_all_tabs
#' @param search_str A character vector of texts. A table is kept if its name
#'   contains at least one of them. Obligatorisk.
#'
#' @return A data frame with the columns `tabellid`, `tab_name` and `updated`,
#'   as [pxr_get_all_tabs()]. Empty if no table matches.
#' @seealso [pxr_get_all_tabs()]
#' @export
#' @example inst/examples/pxr_search_tabs.R
pxr_search_tabs <- function(provider = "scb", updated_since = NULL, lang = NULL, search_str) {
  if (missing(search_str) || is.null(search_str) || length(search_str) == 0 ||
      anyNA(search_str) || !is.character(search_str)) {
    stop("search_str must be a character vector with at least one text", call. = FALSE)
  }
  out <- .pxr_tabs(provider, updated_since, lang)
  keep <- .pxr_filter(ifelse(is.na(out$tab_name), "", out$tab_name), contains = search_str)
  out <- out[keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}

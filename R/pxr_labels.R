# Labels and codes of a table's variables, with simple text filters.

# provider: provider id ("scb", "ssb", ...) or URL -> metadata
.pxr_metadata <- function(provider, tab, lang = NULL) {
  if (missing(provider) || is.null(provider)) stop("provider must be supplied", call. = FALSE)
  if (missing(tab) || is.null(tab)) stop("tab must be supplied", call. = FALSE)
  if (is.list(tab)) return(tab)
  pxweb2_get_metadata(unique(tab), base_url = pxweb2_base_url(provider), lang = lang)
}

# The dimensions in the table's order (metadata$id)
.pxr_dims <- function(metadata) {
  ids <- unlist(metadata$id)
  if (is.null(ids)) return(metadata$dimension)
  metadata$dimension[ids[ids %in% names(metadata$dimension)]]
}

# Codes and labels of one dimension, in the table's order
.pxr_dim_values <- function(dim) {
  lab <- dim$category$label %||% list()
  idx <- dim$category$index
  codes <- names(lab)
  if (!is.null(idx)) {
    ord <- if (is.list(idx) || !is.null(names(idx))) {
      names(sort(unlist(idx)))
    } else as.character(idx)
    codes <- c(ord[ord %in% codes], setdiff(codes, ord))
  }
  data.frame(code = codes,
             label = vapply(codes, function(k) as.character(lab[[k]]), character(1)),
             stringsAsFactors = FALSE, row.names = NULL)
}

# TRUE if tab is several table ids (a metadata object is a list)
.pxr_is_several <- function(tab) {
  !missing(tab) && !is.null(tab) && !is.list(tab) && length(unique(tab)) > 1
}

.pxr_check_var <- function(var) {
  if (missing(var) || is.null(var) || length(var) != 1 || is.na(var)) {
    stop("var must be one variable code or label", call. = FALSE)
  }
}

# var given as code or label (case-insensitive) -> dimension code, NA if the
# table lacks it
.pxr_match_var <- function(metadata, var) {
  codes <- names(metadata$dimension)
  labels <- vapply(metadata$dimension, function(d) d$label %||% NA_character_, character(1))
  hit <- which(tolower(codes) == tolower(var))
  if (length(hit) == 0) hit <- which(tolower(labels) == tolower(var))
  if (length(hit) == 0) NA_character_ else codes[hit[1]]
}

.pxr_find_var <- function(metadata, var) {
  .pxr_check_var(var)
  code <- .pxr_match_var(metadata, var)
  if (is.na(code)) {
    stop("Variable '", var, "' does not exist in the table. Variables: ",
         paste(names(metadata$dimension), collapse = ", "), call. = FALSE)
  }
  code
}

# TRUE for the elements of x that pass every filter that is not NULL
# (case-insensitive). A filter
# with several strings/lengths keeps x if it matches any of them.
.pxr_filter <- function(x, starts_with = NULL, ends_with = NULL, n_chars = NULL,
                        contains = NULL, not_contains = NULL) {
  x <- tolower(x)
  low <- function(p) if (is.null(p)) NULL else tolower(as.character(p))
  starts_with <- low(starts_with); ends_with <- low(ends_with)
  contains <- low(contains); not_contains <- low(not_contains)
  any_of <- function(pats, f) Reduce(`|`, lapply(pats, f), rep(FALSE, length(x)))
  keep <- rep(TRUE, length(x))
  if (!is.null(starts_with)) keep <- keep & any_of(starts_with, function(p) startsWith(x, p))
  if (!is.null(ends_with)) keep <- keep & any_of(ends_with, function(p) endsWith(x, p))
  if (!is.null(contains)) keep <- keep & any_of(contains, function(p) grepl(p, x, fixed = TRUE))
  if (!is.null(not_contains)) keep <- keep & !any_of(not_contains, function(p) grepl(p, x, fixed = TRUE))
  if (!is.null(n_chars)) keep <- keep & nchar(x) %in% n_chars
  keep
}

.pxr_get <- function(by, provider, tab, var, starts_with, ends_with, n_chars,
                     contains, not_contains, swap, lang) {
  one_table <- function(metadata, code) {
    vals <- .pxr_dim_values(metadata$dimension[[code]])
    keep <- .pxr_filter(vals[[by]], starts_with, ends_with, n_chars, contains, not_contains)
    other <- if (by == "label") "code" else "label"
    vals[[if (isTRUE(swap)) other else by]][keep]
  }
  if (!.pxr_is_several(tab)) {
    metadata <- .pxr_metadata(provider, tab, lang)
    return(one_table(metadata, .pxr_find_var(metadata, var)))
  }
  # several tables: the tables that lack the variable are passed over
  .pxr_check_var(var)
  res <- lapply(unique(tab), function(t) {
    metadata <- .pxr_metadata(provider, t, lang)
    code <- .pxr_match_var(metadata, var)
    if (is.na(code)) NULL else one_table(metadata, code)
  })
  if (all(vapply(res, is.null, TRUE))) {
    stop("Variable '", var, "' does not exist in any of the tables: ",
         paste(unique(tab), collapse = ", "), call. = FALSE)
  }
  unique(unlist(res, use.names = FALSE))
}

# One element per variable with the values of `by`. With several tables, the
# variables and their values are combined in the order they first appear.
.pxr_get_all <- function(by, provider, tab, lang) {
  one_table <- function(t) {
    metadata <- .pxr_metadata(provider, t, lang)
    lapply(.pxr_dims(metadata), function(d) .pxr_dim_values(d)[[by]])
  }
  if (!.pxr_is_several(tab)) return(one_table(tab))
  res <- lapply(unique(tab), one_table)
  vars <- unique(unlist(lapply(res, names), use.names = FALSE))
  stats::setNames(lapply(vars, function(v) {
    unique(unlist(lapply(res, `[[`, v), use.names = FALSE))
  }), vars)
}

#' Labels of a table variable, with text filters
#'
#' Fetches the labels (value texts) that a variable in a table can take, and
#' keeps those that match the filters. All filters that are given must match.
#' A filter with several strings keeps a label that matches any of them. The
#' matching is literal (not regular expressions) and does not distinguish between
#' upper and lower case.
#'
#' @param provider The statistics provider, by default `"scb"`. Leave it out by
#'   naming the other arguments, e.g. `tab = "TAB638"`. It can also be a URL to a
#'   PxWeb API v1 or v2; a provider with both versions is always used through v2.
#'   The names are case-insensitive:
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
#' @param tab Table id, e.g. `"TAB638"`, or a vector of table ids, e.g.
#'   `c("TAB5557", "TAB638")`, whose values are then combined without duplicates.
#'   In PxWeb API v1 the table's path below the provider's URL, e.g.
#'   `"indikatorer/Indikatorm.px"`. An id without folders, e.g. `"POPU02"` with or
#'   without `.px` and in upper or lower case, is looked up in the table list. It is
#'   an error if tables in several folders have that name; the error lists their
#'   paths. Obligatorisk.
#' @param var Variable code or label (case-insensitive), e.g. `"Region"`. With
#'   several tables, only those that have the variable contribute values; it is an
#'   error if none has it. Obligatorisk.
#' @param starts_with Keep labels that start with this text (character).
#' @param ends_with Keep labels that end with this text (character).
#' @param n_chars Keep labels with this number of characters (numeric).
#' @param contains Keep labels that contain this text (character).
#' @param not_contains Drop labels that contain this text (character).
#' @param to_codes If `TRUE`, return the codes of the labels that were kept
#'   instead of the labels.
#' @param lang Language code; `NULL` = `getOption("pxweb2r.lang")` or the
#'   provider's default language.
#'
#' @return A character vector of labels, or of codes if `to_codes = TRUE`, in
#'   the table's order. Empty if nothing matches.
#' @seealso [pxr_get_codes()], [pxr_get_all_labels()]
#' @export
#' @example inst/examples/pxr_get_labels.R
pxr_get_labels <- function(provider = "scb", tab, var, starts_with = NULL, ends_with = NULL,
                           n_chars = NULL, contains = NULL, not_contains = NULL,
                           to_codes = FALSE, lang = NULL) {
  .pxr_get("label", provider, tab, var, starts_with, ends_with, n_chars,
           contains, not_contains, to_codes, lang)
}

#' Codes of a table variable, with text filters
#'
#' Fetches the codes that a variable in a table can take, and keeps those that
#' match the filters. All filters that are given must match. A filter with
#' several strings keeps a code that matches any of them. The matching is
#' literal (not regular expressions) and does not distinguish between
#' upper and lower case.
#'
#' @inheritParams pxr_get_labels
#' @param starts_with Keep codes that start with this text (character).
#' @param ends_with Keep codes that end with this text (character).
#' @param n_chars Keep codes with this number of characters (numeric), e.g.
#'   `4` for municipality codes in Sweden.
#' @param contains Keep codes that contain this text (character).
#' @param not_contains Drop codes that contain this text (character).
#' @param to_labels If `TRUE`, return the labels of the codes that were kept
#'   instead of the codes.
#'
#' @return A character vector of codes, or of labels if `to_labels = TRUE`, in
#'   the table's order. Empty if nothing matches.
#' @seealso [pxr_get_labels()], [pxr_get_all_codes()]
#' @export
#' @example inst/examples/pxr_get_codes.R
pxr_get_codes <- function(provider = "scb", tab, var, starts_with = NULL, ends_with = NULL,
                          n_chars = NULL, contains = NULL, not_contains = NULL,
                          to_labels = FALSE, lang = NULL) {
  .pxr_get("code", provider, tab, var, starts_with, ends_with, n_chars,
           contains, not_contains, to_labels, lang)
}

#' All labels of a table
#'
#' Fetches the labels that every variable in a table can take.
#'
#' @inheritParams pxr_get_labels
#'
#' @return A named list with one element per variable (named by the variable
#'   code), each a character vector of the labels the variable can take. With
#'   several tables, the list has every variable that some table has, with the
#'   labels of all the tables that have it, without duplicates.
#' @seealso [pxr_get_labels()], [pxr_get_all_codes()]
#' @export
#' @example inst/examples/pxr_get_all_labels.R
pxr_get_all_labels <- function(provider = "scb", tab, lang = NULL) {
  .pxr_get_all("label", provider, tab, lang)
}

#' All codes of a table
#'
#' Fetches the codes that every variable in a table can take.
#'
#' @inheritParams pxr_get_labels
#'
#' @return A named list with one element per variable (named by the variable
#'   code), each a character vector of the codes the variable can take. With
#'   several tables, the list has every variable that some table has, with the
#'   codes of all the tables that have it, without duplicates.
#' @seealso [pxr_get_codes()], [pxr_get_all_labels()]
#' @export
#' @example inst/examples/pxr_get_all_codes.R
pxr_get_all_codes <- function(provider = "scb", tab, lang = NULL) {
  .pxr_get_all("code", provider, tab, lang)
}

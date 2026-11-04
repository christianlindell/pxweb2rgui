#' Convert an API GET URL into a table id and a query
#'
#' Extracts the table id, `valuecodes`, code lists and language from a PxWeb
#' API v2 GET URL, e.g. the one Statistics Sweden shows under Spara ->
#' API-uttag -> GET, and converts them to arguments for [pxr_pxweb2_get_data()].
#' 
#' @param url A SCB GET API URL. Located under Spara -> API-uttag -> GET. Obligatorisk.
#' 
#' @return A list with:
#' \describe{
#'  \item{table}{Table id.}
#'  \item{query}{Query list suitable for pxweb_get_data().}
#'  \item{lang}{The language in the URL (`lang=`), or `NULL`.}
#'  \item{codelist}{Named character vector of the code lists in the URL
#'    (`codelist[...]`), or `NULL`.}
#'  \item{include_aggregations}{The aggregation code lists (`agg_...`) among
#'    them, to pass as `include_aggregations` to pxr_pxweb2_get_data(); `NULL` if
#'    there are none. Without it, codes from an aggregation are not found.}
#' }
#' 
#' @noRd
pxweb2_GET_url_to_px <- function(url){
  
  table <- sub(".*/tables/([^/]+)/data.*", "\\1", url)
  
  params <- strsplit(sub("^[^?]*\\?", "", url), "&")[[1]]
  # brackets may be percent-encoded when the URL was copied from a browser
  params <- sub("%5B", "[", params, ignore.case = TRUE)
  params <- sub("%5D", "]", params, ignore.case = TRUE)

  param_values <- function(prefix) {
    x <- grep(paste0("^", prefix, "\\["), params, value = TRUE)
    keys <- utils::URLdecode(sub(paste0("^", prefix, "\\[([^]]+)\\]=.*$"), "\\1", x))
    vals <- utils::URLdecode(sub(paste0("^", prefix, "\\[[^]]+\\]=(.*)$"), "\\1", x))
    stats::setNames(vals, keys)
  }

  vals <- param_values("valuecodes")
  query <- lapply(unname(vals), function(x){
    if(grepl(",", x, fixed = TRUE)){
      trimws(strsplit(x, ",", fixed = TRUE)[[1]])
    } else {
      x
    }
  })
  names(query) <- names(vals)

  codelist <- param_values("codelist")
  agg <- codelist[grepl("^agg_", codelist)]
  lang <- grep("^lang=", params, value = TRUE)

  list(
    table = table,
    query = query,
    lang = if (length(lang) > 0) utils::URLdecode(sub("^lang=", "", lang[1])) else NULL,
    codelist = if (length(codelist) > 0) codelist else NULL,
    include_aggregations = if (length(agg) > 0) agg else NULL
  )
  
}

# Package dependencies are declared in DESCRIPTION (Imports) and NAMESPACE, not here.

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


# Minimal local check of a table id before it is put into a URL.
# Only checks that it is a non-empty string of length 1 without whitespace
# or '/' - i.e. catches a passed vector, NULL, NA or a whole URL.
# No naming-convention check ("TAB..."); whether the id actually exists is
# decided by the API via pxweb2_get_metadata() (404 => not on the base_url used).
# In PxWeb API v1 the table id is a path, so '/' and spaces are allowed there.
.pxweb2_check_table_id <- function(table_id, base_url = NULL) {
  if (is.null(table_id) || length(table_id) != 1 || !is.character(table_id) ||
      is.na(table_id) || !nzchar(trimws(table_id))) {
    stop("table id must be a non-empty string of length 1.", call. = FALSE)
  }
  if (.pxweb1_is_url(base_url)) {
    if (grepl("^https?://", table_id)) {
      stop("table id '", table_id, "' is a URL. Pass the table's path below base_url, ",
           "e.g. \"BE/BE0101/BE0101A/BefolkningNy\".", call. = FALSE)
    }
    return(invisible(table_id))
  }
  if (stringr::str_detect(table_id, "\\s|/")) {
    stop(
      "table id '", table_id, "' contains whitespace or '/'. ",
      "Pass only the table id, not a URL or path. A path is a table id in PxWeb API v1, ",
      "but this provider has API v2, which is always used and has other ids; ",
      "find the table with pxr_search_tabs().",
      call. = FALSE
    )
  }
  invisible(table_id)
}

#' Get data from a PxWeb API
#'
#' Fetches data from one or more PxWeb tables (by default Statistics Sweden's
#' statistical database) and returns a tidy `tibble`. Handles, among other
#' things, automatic chunking of large requests, label values in queries,
#' wildcards, "latest period", and splitting DeSO/RegSO by municipality.
#'
#' Both PxWeb API v1 and v2 work. A provider that has both is always used through
#' v2, also if `base_url` is the address of its v1; a message says so. Statistics
#' Denmark (`"dst"`) has an API of its own, which works too. It has no code lists
#' either, so `include_aggregations` gives a warning and is ignored there. API v1
#' has no code lists, so `include_aggregations` gives a warning and is ignored
#' there. For the same reason, the column `kommun` that
#' `split_deso_regso_by_municipality` adds is empty for DeSO areas, also with a
#' warning; `kommun_kod` is complete, and RegSO areas get both. A few API v1
#' tables exist but cannot be delivered as JSON-stat by the server, which gives
#' an error that says so.
#'
#' With several tables, each table is called with the variables and values that
#' exist in it, and the results are combined with [dplyr::bind_rows()]. If `Tid =
#' c("2023", "2024", "2025")` is asked for, and one table has 2023 and 2024 and
#' another has 2025, the first is called with 2023 and 2024 and the second with
#' 2025. Variables that a table lacks are left out of its call. If a table has none
#' of the values asked for in a variable it can eliminate, it is called without the
#' variable, so its rows cover all values of that variable: "totalt, samtliga
#' civilstånd" in one table and no such value in another give the total in both. A
#' table is not called if it has none of the values of a variable it requires, if
#' such a variable is `NA`, or if it has none of the values of a region, where
#' leaving it out would give the whole country. Messages say what
#' was done. Columns that only some tables have are `NA` in the rows of the other
#' tables, as with [dplyr::bind_rows()], and a warning says which.
#'
#' @param table Table id (e.g. `"TAB6104"`) or a vector of table ids, e.g.
#'   `c("TAB5557", "TAB638")`. Repeated ids are fetched once. In PxWeb API v1 the
#'   id is the table's path below `base_url`, e.g.
#'   `"indikatorer/Indikatorm.px"`. An id without folders, e.g. `"POPU02"` with or
#'   without `.px` and in upper or lower case, is looked up in the table list. It is
#'   an error if tables in several folders have that name; the error lists their
#'   paths. Obligatorisk.
#' @param query Named list where each element is a variable and the value is the
#'   values to fetch. `NULL` fetches all values. Both codes and labels are
#'   accepted when `allow_label_values = TRUE`. With several tables, a variable
#'   that a table has but `query` does not name gets all its values in that table,
#'   as with a single table; `NA` leaves it out.
#' @param lang Language for metadata and labels, e.g. `"sv"` or `"en"`. `NULL`
#'   (default) uses `getOption("pxweb2r.lang")` or else the provider's default
#'   language.
#' @param output_format Output format from the API, normally `"json-stat2"`.
#' @param base_url The statistics provider, as `provider` in the other `pxr_`
#'   functions. Default `getOption("pxweb2r.base_url")` or else Statistics Sweden.
#'   It can also be the URL of a PxWeb API: for API v2 the API root or tables
#'   endpoint, for API v1 the level with the language and database, e.g.
#'   `"https://statistik.konj.se/PxWeb/api/v1/sv/KonjBar/"`. The names are:
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
#' @param on_all_values_invalid What happens when every supplied value for a
#'   variable is invalid: `"stop"`, `"*"` or `"null"`. With several tables, this
#'   only applies when no table can be called, except `"*"`, which gives a table
#'   that lacks the values all its values.
#' @param allow_label_values If `TRUE`, queries may contain labels instead of
#'   codes.
#' @param strip_code_in_label Removes codes that also appear in the label column,
#'   for regions and variables whose metadata says that the code is shown with the
#'   value. Other labels are left as they are, e.g. `"20 år"` with the code `20`.
#' @param latest_period_code Code that in a query means "fetch the latest time
#'   period". With several tables, each table gets its own latest period. It only
#'   applies to a table's time variable; elsewhere it is an ordinary code. `NULL`
#'   disables the feature.
#' @param allow_api_wildcards If `TRUE`, `"*"` in queries is passed on as an API
#'   wildcard instead of being matched against valid values.
#' @param harmonise_variable_names Named vector for renaming variables so that
#'   several tables get the same variable name, e.g. `c(Region = "Kommun")`.
#' @param deso_regso_versions Handling of multiple DeSO/RegSO versions:
#'   `"latest"`, `"sum"` or `NULL`.
#' @param split_deso_regso_by_municipality If `TRUE`, `kommun_kod` and `kommun`
#'   are added as separate columns.
#' @param include_aggregations Controls fetching of aggregations: `"auto"`,
#'   `"all"`, `"none"`, `"codelists"`, or a named vector per variable. Not
#'   available in PxWeb API v1 or at Statistics Denmark, where any other value
#'   than `"none"` gives a warning.
#' @param auto_limit Max number of code-list calls when
#'   `include_aggregations = "auto"`.
#' @param code_variables Variables that also get a column with codes. By
#'   default (`NULL`) only labels are returned. Give a character vector of
#'   variable codes, e.g. `c("Region", "Kon")` (recommended in scripts: plain
#'   ASCII and the same in every language), or of labels, e.g.
#'   `c("region", "kön")`, or a numeric vector of positions in the table's
#'   variable order, e.g. `c(1, 3)`. `"*"` gives codes for every variable. The
#'   code column is named after the variable with the suffix `_kod`, e.g.
#'   `region_kod`, and is placed just before the label column. When several
#'   tables are fetched at once, positions are resolved per table.
#' @param quiet If `TRUE`, suppresses the informational messages this function
#'   (and the calls it makes internally) would otherwise print -
#'   the `include_aggregations = "auto"` summary, notices about tables that are
#'   skipped or fetched without a variable when fetching several tables, and the
#'   "invalid values removed" notices from `on_all_values_invalid`. Genuine problems (a table that does
#'   not exist, an unknown variable name, an `on_all_values_invalid = "stop"`
#'   error, mismatched result structures across tables) are never suppressed -
#'   `quiet` only silences messages about things the query was already
#'   designed to expect, which is typically the case in a finished, tested
#'   script. Default `FALSE` keeps today's behaviour.
#'
#' @return A `tibble` with the fetched data. With several tables, the first
#'   column `table_id` says which table a row comes from.
#' @export
#' @example inst/examples/pxr_pxweb2_get_data.R
pxr_pxweb2_get_data <- function(
    table,
    query = NULL,               # a list where each name is a variable and the value is the values wanted; values may be vectors
    lang = NULL,                             # language code, e.g. "sv" or "en"; NULL = getOption("pxweb2r.lang") or the provider's default language
    output_format = "json-stat2",
    base_url = .pxweb2_default_base_url(),   # provider id (see pxweb2_providers()) or URL to a PxWeb API v2
    on_all_values_invalid = "stop",          # if all values for a variable are invalid: "stop" halts the request, "*" returns all values for the variable, "null" returns NULL (handy when fetching from several tables where different years or regions live in different tables)
    allow_label_values = TRUE,               # if TRUE, labels may be used for values in queries, i.e. both "20" and "Dalarnas lan" work
    strip_code_in_label = TRUE,              # removes codes that also appear in the label column. If only a code exists and no label, the code is kept as the label
    latest_period_code = "9999",             # this value fetches the latest value of the time column; NULL = not used
    allow_api_wildcards = TRUE,              # TRUE = if the query contains "*" the API's built-in wildcard is used; FALSE = "*" is treated as an ordinary value and matched against valid values in the table
    harmonise_variable_names = NULL,         # used when fetching several tables at once. Pass a named vector, e.g. c("Region" = "Kommun"), and the variable "Kommun" is renamed to "Region" so all tables share the same variable name for municipalities and municipality codes
    deso_regso_versions = "latest",          # if there are several DeSO/RegSO versions, "latest" uses the most recent version that has a value, "sum" sums all versions, NULL does nothing
    split_deso_regso_by_municipality = TRUE, # adds the columns kommun_kod and kommun and keeps only the RegSO/DeSO name in the region column
    include_aggregations = "none",           # controls aggregations in valid_values_list (see pxweb2_get_values): "none"/FALSE = none, "all"/TRUE = all, "auto" = auto, "codelists" = metadata only, or a named vector c(Region = "agg_RegionLA2018")
    auto_limit = 30L,                        # max total number of code-list calls when include_aggregations = "auto"
    code_variables = NULL,                   # variables that also get a code column "<label>_kod": labels, codes or positions; NULL = labels only, "*" = all
    quiet = FALSE                            # TRUE suppresses informational messages (include_aggregations/"auto", latest-period substitution, "invalid values removed") - never suppresses errors or genuine warnings
){
  if (is.null(table)) stop("table_id must be supplied")
  if (!on_all_values_invalid %in% c("stop", "*", "null")) {
    stop("Invalid value for on_all_values_invalid. Allowed: \"stop\", \"*\", \"null\".")
  }
  if (!is.list(table)) table <- unique(table)
  base_url <- .pxweb2_normalise_base_url(base_url)
  lang <- .pxweb2_resolve_lang(lang, base_url)
  is_v1 <- .pxweb1_is_url(base_url)
  is_dst <- .dst_is_url(base_url)
  if (is_v1) include_aggregations <- .pxweb1_no_aggregations(include_aggregations)
  if (is_dst) {
    include_aggregations <- .pxweb1_no_aggregations(include_aggregations, "Statistics Denmark's API")
  }

  # check whether more than one table was requested
  if (!is.list(table) && length(table) > 1) {
    return(
      .pxweb2_get_multiple_tables(
        tables = table,
        query = query,
        lang = lang,
        output_format = output_format,
        base_url = base_url,
        on_all_values_invalid = on_all_values_invalid,
        allow_label_values = allow_label_values,
        strip_code_in_label = strip_code_in_label,
        latest_period_code = latest_period_code,
        allow_api_wildcards = allow_api_wildcards,
        harmonise_variable_names = harmonise_variable_names,
        deso_regso_versions = deso_regso_versions,
        split_deso_regso_by_municipality = split_deso_regso_by_municipality,
        include_aggregations = include_aggregations,
        auto_limit = auto_limit,
        code_variables = code_variables,
        quiet = quiet
      )
    )
  }
  
  if (!is.null(deso_regso_versions)) {
    deso_regso_versions <- match.arg(
      deso_regso_versions,
      choices = c("latest", "sum")
    )
  }
  
  # the API's own limits (max cells per call, calls per time window); also
  # sets the rate limiter. Fetched once per session and base_url.
  config <- .pxweb2_get_config(base_url)

  if (!is.list(table)) {
    .pxweb2_check_table_id(table, base_url)
    metadata <- pxweb2_get_metadata(table, base_url = base_url, lang = lang)
    # in API v1 an id without folders has been turned into the table's path
    if (is_v1) table <- metadata$extension$px$tableid
  } else {
    metadata <- table
    table <- metadata$extension$px$tableid
  }

  data_url <- if (is_v1) {
    .pxweb1_url(base_url, lang, table)
  } else if (is_dst) {
    paste0(base_url, "data")
  } else {
    paste0(base_url, table, "/data")
  }

  variables_df <- pxweb2_get_variables(metadata)          # fetch all variables
  if (is_dst) config$maxDataCells <- .dst_max_rows(variables_df)
  keep_code_vars <- .pxweb2_resolve_code_variables(variables_df, code_variables)
  valid_values_list <- pxweb2_get_values(metadata, include_aggregations = include_aggregations, auto_limit = auto_limit, quiet = quiet)      # fetch all unique values for all variables (incl. aggregations if selected)
  
  # build a query list from the supplied query, or one covering all valid values
  query_list <- if (is.null(query)) {
    .pxweb2_create_variable_query_list(variables_df)
  } else {
    if (.pxweb2_is_pxweb_query_list(query)) {
      query
    } else {
      .pxweb2_list_to_query_list(
        variables_df,
        query,
        valid_values_list = valid_values_list,
        allow_label_values = allow_label_values
      )
    }
  }
  
  query_list <- query_list |>
    .pxweb2_resolve_latest_period(
      variables_df = variables_df,
      valid_values_list = valid_values_list,
      latest_period_code = latest_period_code
    ) |>
    .pxweb2_resolve_api_wildcards(
      valid_values_list = valid_values_list,
      allow_api_wildcards = allow_api_wildcards
    ) |>
    .pxweb2_sanitize_query_values(
      valid_values_list = valid_values_list,
      on_all_values_invalid = on_all_values_invalid,
      warn = !quiet
    )
  
  
  # if query_list = NULL, NULL is returned - useful when fetching a set of values
  # for variables that live in different tables

  # expand any aggregations / "**" into several requests
  if (any(purrr::map_lgl(query_list$selection, ~ is.null(.x$valueCodes)))) {
    return(NULL)
  }

  request_list <- .pxweb2_expand_requests_generic(query_list, valid_values_list)


  # drop requests that lack a selection or are NULL
  request_list <- purrr::compact(request_list)
  if (length(request_list) == 0) return(NULL)

  # split into chunks if the request has more than 150,000 cells
  query_chunks <- .pxweb2_make_request_chunks(
    variables_df,
    request_list,
    valid_values_list = valid_values_list,
    max_cells = config$maxDataCells
  )

  if (!quiet && length(query_chunks) > config$maxCallsPerTimeWindow) {
    minutes <- ceiling(length(query_chunks) / config$maxCallsPerTimeWindow * config$timeWindow / 60)
    message(
      "The request is split into ", length(query_chunks), " calls (max ",
      format(config$maxDataCells, big.mark = " "), " cells per call, ",
      config$maxCallsPerTimeWindow, " calls per ", config$timeWindow,
      " seconds). This takes at least ", minutes, " minute(s)."
    )
  }


  # 1) determine which code columns to fetch: the ones the user asked for, plus
  #    the ones needed internally to strip codes from labels and to handle
  #    DeSO/RegSO. The internal ones are dropped again at the end. Variables
  #    that are eliminated are not in the response.
  selected_vars <- purrr::map_chr(query_list$selection, "variableCode")
  cols_to_add <- variables_df |>
    dplyr::filter(
      code %in% keep_code_vars |
        show == "code_value" |
        role == "geo" |
        stringr::str_to_lower(code) == "region" |
        stringr::str_to_lower(label) == "region"
    ) |>
    dplyr::filter(code %in% selected_vars) |>
    dplyr::transmute(
      code_col = code,
      txt_col = label,
      new_name = paste0(label, "_kod"),
      keep = code %in% keep_code_vars,
      # bara där namnet är "kod namn", inte "20 år" (kod 20), där talet är en del av namnet
      strip = .pxweb2_label_has_code(code, label, role, show)
    )
  
  # all data is fetched here ============
  result_table <- purrr::map(query_chunks, function(req) {

    # always POST: code lists go into the body's selection (the "codelist"
    # field), and outputValues[...] is sent as a query parameter
    post <- .pxweb2_post_request(req)

    data_resp <- if (is_v1) {
      # the language is in the URL, and the body has another form than in v2
      .pxweb2_POST(
        data_url,
        body = jsonlite::toJSON(.pxweb1_post_body(post$body, valid_values_list, output_format),
                                auto_unbox = TRUE),
        encode = "raw",
        httr::content_type_json(),
        httr::accept_json()
      )
    } else if (is_dst) {
      .pxweb2_POST(
        data_url,
        body = jsonlite::toJSON(.dst_post_body(post$body, table, lang, valid_values_list),
                                auto_unbox = TRUE),
        encode = "raw",
        httr::content_type_json(),
        httr::accept_json()
      )
    } else {
      .pxweb2_POST(
        data_url,
        body = jsonlite::toJSON(post$body, auto_unbox = TRUE),
        query = c(list(lang = lang, outputFormat = output_format), post$query),
        encode = "raw",
        httr::content_type_json(),
        httr::accept_json()
      )
    }

    # the metadata was found, so the table exists: some v1 sites give 404 on
    # JSON-stat for a table that they can deliver in other formats
    if (is_v1 && httr::status_code(data_resp) == 404) {
      stop(
        "Table '", table, "' exists on ", base_url, ", but the server could not deliver ",
        "its data as JSON-stat (HTTP 404), which is the only format this package reads. ",
        "The fault is on the server. The data may be available in other formats through ",
        "the provider's website or its API.",
        call. = FALSE
      )
    }

    if (httr::status_code(data_resp) >= 400) {
      cat("HTTP ", httr::status_code(data_resp), "\n", sep = "")
      cat(httr::content(data_resp, "text", encoding = "UTF-8"), "\n", sep = "")
    }
    httr::stop_for_status(data_resp)

    json_text <- .pxweb1_strip_bom(httr::content(data_resp, "text", encoding = "UTF-8"))

    # the response is parsed once; labels and the code columns we need are
    # built from the same parse
    parsed <- .pxweb2_parse_jsonstat2(json_text, code_dims = cols_to_add$code_col)
    df_labels <- parsed$labels

    # the data comes in `lang`, but variables_df is built from metadata that
    # may be in another language: name the code columns after the labels in
    # the response, e.g. "sex_kod" instead of "kön_kod" when lang = "en"
    if (nrow(cols_to_add) > 0 && !is.null(parsed$dim_labels)) {
      resp_lab <- unname(parsed$dim_labels[cols_to_add$code_col])
      ok <- !is.na(resp_lab)
      cols_to_add$txt_col[ok] <<- resp_lab[ok]
      cols_to_add$new_name[ok] <<- paste0(resp_lab[ok], "_kod")
    }

    # if there are columns we should also fetch codes for, they are added here
    df_codes <- if (nrow(cols_to_add) > 0) {
      parsed$codes |>
        dplyr::select(dplyr::all_of(stats::setNames(cols_to_add$code_col, cols_to_add$new_name)))
    } else NULL

    # combine label and code columns
    df_result <- dplyr::bind_cols(purrr::compact(list(df_codes, df_labels)))

    if (isTRUE(strip_code_in_label) && nrow(cols_to_add) > 0) {
      df_result <- purrr::reduce(which(cols_to_add$strip), function(acc, i) {

        code_col <- cols_to_add$new_name[i]
        text_col <- cols_to_add$txt_col[i]

        if (!all(c(code_col, text_col) %in% names(acc))) {
          return(acc)
        }


        acc[[text_col]] <- .pxweb2_strip_code_in_label(
          label = acc[[text_col]],
          code = acc[[code_col]]
        )

        acc

      }, .init = df_result)

    }

    return(df_result)

  }, .progress = TRUE) |>
    purrr::list_rbind()

  # handle DeSO/RegSO versions here if requested (value "latest" or "sum", not NULL)
  result_table <- .pxweb2_handle_deso_regso_versions(
    result_table,
    mode = deso_regso_versions,
    value_col = "value"
  )

  # optionally split municipality code and municipality into their own columns and
  # keep only the RegSO name (and DeSO code as name) in the region column
  if (isTRUE(split_deso_regso_by_municipality)) {
    result_table <- .pxweb2_split_deso_regso_municipality(result_table)

    result_table <- .pxweb2_fill_deso_municipality_from_metadata(
      df = result_table,
      metadata = metadata,
      base_url = base_url
    )

    if (is_v1 && all(c("kommun", "kommun_kod") %in% names(result_table)) &&
        any(is.na(result_table$kommun) & !is.na(result_table$kommun_kod))) {
      warning("PxWeb API v1 has no code lists, so the names of the municipalities are ",
              "missing in the column kommun. The column kommun_kod is complete.", call. = FALSE)
    }
  }

  # drop the code columns that were only needed internally
  result_table <- dplyr::select(
    result_table,
    -dplyr::any_of(cols_to_add$new_name[!cols_to_add$keep])
  )
  cols_to_add <- cols_to_add[cols_to_add$keep, ]

  # move each code column to just before its label column
  df_result <- purrr::reduce(seq_len(nrow(cols_to_add)), function(acc, i) {
    dplyr::relocate(acc, dplyr::all_of(cols_to_add$new_name[i]),
                    .before = dplyr::all_of(cols_to_add$txt_col[i]))
  }, .init = result_table)


  return(df_result)

}

#' Get when a table was last updated
#'
#' @param table Table id or metadata object (its table id is used, and the
#'   API is always asked again). Obligatorisk.
#' @param base_url Base URL of the PxWeb API v2.
#'
#' @return Timestamp (ISO 8601) as a string, e.g. `"2026-06-02T00:59:31Z"`, or
#'   `NA` if the value is missing.
#' @noRd
pxweb2_table_updated <- function(
    table,
    base_url = .pxweb2_default_base_url()
) {
  if (is.list(table)) table <- table$extension$px$tableid
  base_url <- .pxweb2_normalise_base_url(base_url)
  if (.pxweb1_is_url(base_url)) {
    return(.pxweb1_table_updated(table, base_url, .pxweb2_resolve_lang(NULL, base_url)))
  }
  # always ask the API - a cached copy could hide a fresh update
  pxweb2_get_metadata(table, base_url = base_url, use_cache = FALSE)$updated
}

#' Check whether a table needs updating
#'
#' Compares the table's update time in PxWeb with a timestamp of your own.
#'
#' @param table Table id. Obligatorisk.
#' @param reference_datetime Timestamp to compare against, format
#'   `"YYYY-MM-DDTHH:MM:SSZ"`. Obligatorisk.
#' @param base_url Base URL of the PxWeb API v2.
#'
#' @return `TRUE` if the PxWeb table is newer than `reference_datetime`, `FALSE`
#'   otherwise, or `NA` if the table's update value is missing.
#' @noRd
pxweb2_table_needs_update <- function(
    table,
    reference_datetime,                 # date + time to compare against, in the same format as Statistics Sweden's `updated` in the metadata,
                                        # namely: 2026-06-02T00:59:31Z
    base_url = .pxweb2_default_base_url()
) {

  base_url <- .pxweb2_normalise_base_url(base_url)
  .pxweb2_check_table_id(table, base_url)

  if (is.null(reference_datetime) || length(reference_datetime) != 1) {
    stop("reference_datetime must be a text value of length 1.", call. = FALSE)
  }

  format_ok <- "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$"

  if (!stringr::str_detect(reference_datetime, format_ok)) {
    stop(
      "reference_datetime must have the format 'YYYY-MM-DDTHH:MM:SSZ', e.g. '2025-05-28T06:00:00Z'.",
      call. = FALSE
    )
  }

  scb_updated <- pxweb2_table_updated(table, base_url = base_url)

  if (is.na(scb_updated)) {
    return(NA)
  }

  if (!stringr::str_detect(scb_updated, format_ok)) {
    stop(
      "The API's `updated` value does not have the expected format: ",
      scb_updated,
      call. = FALSE
    )
  }

  scb_updated > reference_datetime
} # end function pxweb2_table_needs_update


# split municipality code and municipality name into their own columns, and reduce
# the region column to just the RegSO/DeSO name (the DeSO name equals its code)
.pxweb2_split_deso_regso_municipality <- function(df,
                                                    region_col = "region",
                                                    region_code_col = "region_kod") {
  
  if (!all(c(region_col, region_code_col) %in% names(df))) {
    return(df)
  }
  
  region_koder <- as.character(df[[region_code_col]])
  
  grund_region_koder <- dplyr::if_else(
    stringr::str_detect(stringr::str_to_lower(region_koder), "deso|regso"),
    stringr::str_remove(region_koder, "_.*$"),
    region_koder
  )
  
  har_deso_regso <- stringr::str_detect(
    grund_region_koder,
    "^\\d{4}[ABC]\\d+|^\\d{4}R\\d+"
  ) |
    stringr::str_detect(
      stringr::str_to_lower(region_koder),
      "deso|regso"
    )
  
  if (!any(har_deso_regso, na.rm = TRUE)) {
    return(df)
  }
  
  df |>
    dplyr::mutate(
      .region_tmp = as.character(.data[[region_col]]),
      .region_kod_tmp = as.character(.data[[region_code_col]]),
      
      .grund_region_kod = dplyr::if_else(
        stringr::str_detect(stringr::str_to_lower(.region_kod_tmp), "deso|regso"),
        stringr::str_remove(.region_kod_tmp, "_.*$"),
        .region_kod_tmp
      ),
      
      .ar_deso = stringr::str_detect(.grund_region_kod, "^\\d{4}[ABC]\\d+"),
      .ar_regso = stringr::str_detect(.grund_region_kod, "^\\d{4}R\\d+"),
      .ar_deso_regso = .ar_deso | .ar_regso,
      
      .region_text_ren = .region_tmp,
      .region_text_ren = stringr::str_remove(
        .region_text_ren,
        paste0("^", stringr::str_escape(.region_kod_tmp), "\\s+")
      ),
      .region_text_ren = stringr::str_remove(
        .region_text_ren,
        paste0("^", stringr::str_escape(.grund_region_kod), "\\s+")
      ),
      .region_text_ren = stringr::str_trim(.region_text_ren),
      
      .har_parentes = stringr::str_detect(.region_text_ren, "\\([^()]+\\)"),
      
      kommun_kod = dplyr::if_else(
        .ar_deso_regso & stringr::str_detect(.grund_region_kod, "^\\d{4}"),
        stringr::str_sub(.grund_region_kod, 1, 4),
        NA_character_
      ),
      
      kommun = dplyr::case_when(
        .har_parentes ~ stringr::str_trim(
          stringr::str_remove(.region_text_ren, "\\s*\\([^()]+\\)\\s*$")
        ),
        .ar_deso & .region_text_ren != "" & .region_text_ren != .grund_region_kod ~ .region_text_ren,
        TRUE ~ NA_character_
      ),
      
      "{region_col}" := dplyr::case_when(
        .har_parentes ~ stringr::str_match(.region_text_ren, "\\(([^()]+)\\)")[, 2],
        .ar_deso ~ .grund_region_kod,
        .ar_regso & .region_text_ren != "" ~ .region_text_ren,
        .ar_regso ~ .grund_region_kod,
        TRUE ~ .region_tmp
      ),
      
      "{region_code_col}" := dplyr::if_else(
        .ar_deso_regso,
        .grund_region_kod,
        .region_kod_tmp
      )
    ) |>
    dplyr::select(
      -.region_tmp,
      -.region_kod_tmp,
      -.grund_region_kod,
      -.ar_deso,
      -.ar_regso,
      -.ar_deso_regso,
      -.region_text_ren,
      -.har_parentes
    ) |>
    dplyr::relocate(
      dplyr::any_of(c("kommun_kod", "kommun")),
      .after = dplyr::all_of(region_code_col)
    )
}

# handle several different versions of DeSO or RegSO in the same table
.pxweb2_handle_deso_regso_versions <- function(df,
                                                       mode = c("latest", "sum"),
                                                       region_col = "region",
                                                       region_code_col = "region_kod",
                                                       value_col = "value") {
  
  if (is.null(mode)) return(df)
  
  mode <- match.arg(mode)
  
  if (!all(c(region_col, region_code_col, value_col) %in% names(df))) {
    return(df)
  }
  
  tmp <- df |>
    dplyr::mutate(
      .row_id = dplyr::row_number(),
      .region_kod_tmp = as.character(.data[[region_code_col]]),
      .region_tmp = as.character(.data[[region_col]]),
      
      .grund_region_kod = stringr::str_remove(.region_kod_tmp, "_.*$"),
      
      .ar_deso_regso = stringr::str_detect(
        .grund_region_kod,
        "^\\d{4}[ABC]\\d+|^\\d{4}R\\d+"
      ),
      
      .version_ar = stringr::str_match(
        stringr::str_to_lower(.region_kod_tmp),
        "_(?:deso|regso)(\\d{4})"
      )[, 2],
      .version_ar = suppressWarnings(as.integer(.version_ar)),
      .version_ar = dplyr::if_else(is.na(.version_ar), 0L, .version_ar),
      
      .har_version = stringr::str_detect(
        stringr::str_to_lower(.region_kod_tmp),
        "_(?:deso|regso)\\d{4}"
      ),
      
      .value_num = suppressWarnings(as.numeric(.data[[value_col]])),
      .har_data = !is.na(.value_num) & .value_num != 0
    )
  
  tmp_other <- tmp |>
    dplyr::filter(!.ar_deso_regso)

  tmp_deso_regso <- tmp |>
    dplyr::filter(.ar_deso_regso)
  
  if (nrow(tmp_deso_regso) == 0) {
    return(
      tmp |>
        dplyr::select(
          -.row_id,
          -.region_kod_tmp,
          -.region_tmp,
          -.grund_region_kod,
          -.ar_deso_regso,
          -.version_ar,
          -.har_version,
          -.value_num,
          -.har_data
        )
    )
  }
  
  grouping_cols <- setdiff(
    names(tmp_deso_regso),
    c(
      region_code_col,
      region_col,
      value_col,
      "kommun_kod",
      "kommun",
      ".row_id",
      ".region_kod_tmp",
      ".region_tmp",
      ".grund_region_kod",
      ".ar_deso_regso",
      ".version_ar",
      ".har_version",
      ".value_num",
      ".har_data"
    )
  )
  
  grouping_cols <- c(".grund_region_kod", grouping_cols)

  if (mode == "latest") {

    tmp_deso_regso_done <- tmp_deso_regso |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) |>
      dplyr::arrange(
        dplyr::desc(.har_data),
        dplyr::desc(.version_ar),
        dplyr::desc(.har_version),
        .row_id,
        .by_group = TRUE
      ) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        "{region_code_col}" := .grund_region_kod
      )

  } else if (mode == "sum") {

    tmp_deso_regso_done <- tmp_deso_regso |>
      dplyr::mutate(
        "{region_code_col}" := .grund_region_kod
      ) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(region_code_col, grouping_cols)))) |>
      dplyr::arrange(
        dplyr::desc(.har_data),
        dplyr::desc(.version_ar),
        .row_id,
        .by_group = TRUE
      ) |>
      dplyr::summarise(
        "{region_col}" := dplyr::first(.data[[region_col]]),
        "{value_col}" := sum(.value_num, na.rm = TRUE),
        .row_id = dplyr::first(.row_id),
        .groups = "drop"
      )
  }
  
  dplyr::bind_rows(tmp_other, tmp_deso_regso_done) |>
    dplyr::arrange(.row_id) |>
    dplyr::select(
      -dplyr::any_of(c(
        ".row_id",
        ".region_kod_tmp",
        ".region_tmp",
        ".grund_region_kod",
        ".ar_deso_regso",
        ".version_ar",
        ".har_version",
        ".value_num",
        ".har_data"
      ))
    )
}


.pxweb2_find_municipality_valueset_url <- function(metadata,
                                                    region_var = "Region") {
  
  region_dim <- metadata$dimension[[region_var]]
  
  if (is.null(region_dim)) {
    return(NULL)
  }
  
  cl <- purrr::pluck(region_dim, "extension", "codelists", .default = NULL)
  
  if (is.null(cl) || length(cl) == 0) {
    return(NULL)
  }
  
  cl_df <- purrr::map_dfr(cl, function(item) {
    tibble::tibble(
      id = purrr::pluck(item, "id", .default = NA_character_),
      label = purrr::pluck(item, "label", .default = NA_character_),
      type = purrr::pluck(item, "type", .default = NA_character_),
      url = purrr::pluck(item, "links", 1, "href", .default = NA_character_)
    )
  })
  
  hit <- cl_df |>
    dplyr::filter(
      stringr::str_to_lower(type) == "valueset",
      stringr::str_detect(stringr::str_to_lower(label), "kommun") |
        stringr::str_detect(stringr::str_to_lower(id), "kommun")
    )
  # note: "kommun" is Statistics Sweden's own term for municipality and appears
  # verbatim in the API metadata, so it is matched literally here.

  if (nrow(hit) == 0) {
    return(NULL)
  }

  hit$url[[1]]
}


.pxweb2_municipality_key_from_metadata <- function(metadata,
                                                           region_var = "Region") {
  
  url <- .pxweb2_find_municipality_valueset_url(
    metadata = metadata,
    region_var = region_var
  )
  
  if (is.null(url) || is.na(url) || identical(url, "")) {
    return(NULL)
  }
  
  resp <- .pxweb2_GET(url, httr::accept_json())
  httr::stop_for_status(resp)
  
  cl <- jsonlite::fromJSON(
    httr::content(resp, "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
  
  values <- purrr::pluck(cl, "values", .default = NULL)
  
  if (is.null(values) || length(values) == 0) {
    return(NULL)
  }
  
  municipality_key <- purrr::map_dfr(values, function(x) {
    tibble::tibble(
      municipality_code = as.character(purrr::pluck(x, "code", .default = NA_character_)),
      municipality = as.character(purrr::pluck(x, "label", .default = NA_character_))
    )
  }) |>
    dplyr::filter(!is.na(municipality_code), !is.na(municipality)) |>
    dplyr::mutate(
      municipality = .pxweb2_strip_code_prefix_in_label(
        label = municipality,
        code = municipality_code
      )
    ) |>
    dplyr::distinct(municipality_code, .keep_all = TRUE)
  return(municipality_key)
}

.pxweb2_fill_deso_municipality_from_metadata <- function(df,
                                                         metadata,
                                                         region_code_col = "region_kod",
                                                         municipality_code_col = "kommun_kod",
                                                         municipality_col = "kommun",
                                                         base_url = NULL) {
  
  if (!all(c(region_code_col, municipality_code_col, municipality_col) %in% names(df))) {
    return(df)
  }
  
  region_code <- as.character(df[[region_code_col]])

  # both DeSO (e.g. 0114A0010) and RegSO (e.g. 0114R001) get a municipality
  is_deso <- stringr::str_detect(region_code, "^\\d{4}[ABC]\\d+|^\\d{4}R\\d+")

  if (!any(is_deso, na.rm = TRUE)) {
    return(df)
  }

  missing_municipality <- is.na(df[[municipality_col]]) | df[[municipality_col]] == ""

  if (!any(is_deso & missing_municipality, na.rm = TRUE)) {
    return(df)
  }

  # a failing code-list call must not stop the whole data fetch
  municipality_key <- tryCatch(.pxweb2_municipality_key_from_metadata(metadata),
                               error = function(e) NULL)

  # DeSO/RegSO tables often lack a municipality value set of their own; then
  # Statistics Sweden's general municipality code list is used
  if ((is.null(municipality_key) || nrow(municipality_key) == 0) && !is.null(base_url)) {
    municipality_key <- tryCatch({
      cl <- pxweb2_get_codelist("vs_RegionKommun07", base_url = base_url)
      tibble::tibble(
        municipality_code = cl$code,
        municipality = .pxweb2_strip_code_prefix_in_label(cl$label, cl$code)
      )
    }, error = function(e) NULL)
  }

  if (is.null(municipality_key) || nrow(municipality_key) == 0) {
    return(df)
  }

  match_idx <- match(
    as.character(df[[municipality_code_col]]),
    municipality_key$municipality_code
  )

  municipality_from_metadata <- municipality_key$municipality[match_idx]

  fill <- is_deso & missing_municipality & !is.na(municipality_from_metadata)

  df[[municipality_col]][fill] <- municipality_from_metadata[fill]

  df
}


# helper: translate code_variables (labels, codes or positions) into variable codes
.pxweb2_resolve_code_variables <- function(variables_df, code_variables) {

  if (is.null(code_variables) || length(code_variables) == 0) {
    return(character(0))
  }

  if (is.numeric(code_variables)) {
    pos <- as.integer(code_variables)
    bad <- is.na(pos) | pos < 1 | pos > nrow(variables_df) | pos != code_variables
    if (any(bad)) {
      stop(
        "code_variables: position(s) ", paste(code_variables[bad], collapse = ", "),
        " do not exist. The table has ", nrow(variables_df), " variables: ",
        paste0(seq_len(nrow(variables_df)), " = ", variables_df$label, collapse = ", "),
        call. = FALSE
      )
    }
    return(unique(variables_df$code[pos]))
  }

  if (!is.character(code_variables)) {
    stop("code_variables must be a character vector of variable names or a numeric vector of positions.",
         call. = FALSE)
  }

  if ("*" %in% code_variables) {
    return(variables_df$code)
  }

  # match exact variable codes first (e.g. "Kon", the same in every language
  # and plain ASCII), then labels and codes ignoring case
  wanted <- stringr::str_to_lower(code_variables)
  idx <- match(code_variables, variables_df$code)
  idx_label <- match(wanted, stringr::str_to_lower(variables_df$label))
  idx[is.na(idx)] <- idx_label[is.na(idx)]
  idx_code <- match(wanted, stringr::str_to_lower(variables_df$code))
  idx[is.na(idx)] <- idx_code[is.na(idx)]

  if (anyNA(idx)) {
    stop(
      "code_variables: the variable(s) ", paste0('"', code_variables[is.na(idx)], '"', collapse = ", "),
      " do not exist in the table. Available: ",
      paste0('"', variables_df$label, '"', collapse = ", "),
      call. = FALSE
    )
  }

  unique(variables_df$code[idx])
}


# helper: lets code_variables use the standard name from harmonise_variable_names,
# e.g. code_variables = "Region" when a given table has the variable Kommun
.pxweb2_harmonise_code_variables <- function(code_variables,
                                             variables_df,
                                             harmonise_variable_names = NULL) {

  if (!is.character(code_variables) || is.null(harmonise_variable_names)) {
    return(code_variables)
  }

  known <- stringr::str_to_lower(c(variables_df$label, variables_df$code))

  purrr::map_chr(code_variables, function(v) {
    if (stringr::str_to_lower(v) %in% known) return(v)
    pos <- match(stringr::str_to_lower(v), stringr::str_to_lower(names(harmonise_variable_names)))
    if (is.na(pos)) v else unname(harmonise_variable_names[[pos]])
  })
}


.pxweb2_strip_code_prefix_in_label <- function(label, code) {

  label_chr <- as.character(label)
  code_chr <- as.character(code)

  purrr::map2_chr(label_chr, code_chr, function(lbl, k) {

    if (is.na(lbl) || is.na(k)) {
      return(lbl)
    }

    cleaned <- stringr::str_remove(
      lbl,
      paste0("^", stringr::str_escape(k), "\\s+")
    ) |>
      stringr::str_trim()

    if (identical(cleaned, "")) {
      lbl
    } else {
      cleaned
    }
  })
}


# helper: warn about differing structure
.pxweb2_warn_if_different_structure <- function(result_list) {

  columns_list <- result_list |>
    purrr::map(names)

  all_columns <- Reduce(union, columns_list)

  missing_list <- columns_list |>
    purrr::imap(function(columns, name) {
      setdiff(all_columns, columns)
    })

  has_differences <- any(lengths(missing_list) > 0)

  if (isTRUE(has_differences)) {

    details <- missing_list |>
      purrr::imap_chr(function(missing, name) {
        if (length(missing) == 0) {
          paste0(name, ": no missing columns")
        } else {
          paste0(name, ": missing ", paste(missing, collapse = ", "))
        }
      }) |>
      paste(collapse = "\n")

    warning(
      "The tables do not have an identical column structure. ",
      "Missing columns are filled with NA by dplyr::bind_rows().\n",
      details,
      call. = FALSE
    )
  }

  invisible(NULL)
}

# main function for several tables
.pxweb2_get_multiple_tables <- function(
    tables,
    query = NULL,
    lang = NULL,
    output_format = "json-stat2",
    base_url = .pxweb2_default_base_url(),
    on_all_values_invalid = "stop",
    allow_label_values = TRUE,
    strip_code_in_label = TRUE,
    latest_period_code = "9999",
    allow_api_wildcards = TRUE,
    harmonise_variable_names = NULL,
    deso_regso_versions = "latest",
    split_deso_regso_by_municipality = TRUE,
    include_aggregations = "none",
    auto_limit = 30L,
    code_variables = NULL,
    quiet = FALSE
) {

  if (!is.character(tables) || length(tables) < 1) {
    stop("tables must be a character vector with at least one table id.", call. = FALSE)
  }
  base_url <- .pxweb2_normalise_base_url(base_url)
  purrr::walk(tables, .pxweb2_check_table_id, base_url = base_url)
  lang <- .pxweb2_resolve_lang(lang, base_url)

  metadata_list <- tables |>
    purrr::map(\(t) pxweb2_get_metadata(t, base_url = base_url, lang = lang))

  names(metadata_list) <- tables

  # resolve "auto" once, based on the total number of code lists across ALL tables
  if (identical(include_aggregations, "auto")) {
    .cl_count <- function(meta) {
      purrr::map_int(meta$dimension, function(dim_el) {
        cl <- purrr::pluck(dim_el, "extension", "codelists", .default = NULL)
        if (is.null(cl)) 0L else sum(purrr::map_lgl(cl, ~ tolower(purrr::pluck(.x, "type", .default = "")) == "aggregation"))
      }) |> sum()
    }
    total <- purrr::map_int(metadata_list, .cl_count) |> sum()
    include_aggregations <- if (total <= auto_limit) {
      if (!quiet) message(
        "include_aggregations = \"auto\": found ", total, " code lists in total (",
        length(tables), " tables, limit ", auto_limit, ") and is fetching all aggregations."
      )
      "all"
    } else {
      if (!quiet) message(
        "include_aggregations = \"auto\": found ", total, " code lists in total (",
        length(tables), " tables), which exceeds the limit ", auto_limit,
        ". Fetching code-list metadata only (\"codelists\"). Set \"all\" to fetch everything."
      )
      "codelists"
    }
  }

  # "9999" is resolved by each table's own call, for its own time variable
  query_adjusted <- query
  tables_to_fetch <- tables

  # variables and values that some table has, to tell what no table has
  known_names <- character()
  known_values <- list()
  no_match_tables <- character()

  result_list <- tables_to_fetch |>
    purrr::map(function(table_id) {

      metadata_table <- metadata_list[[table_id]]
      variables_df_table <- pxweb2_get_variables(metadata_table)
      aggregations_table <- .pxweb2_aggregations_for_table(include_aggregations, variables_df_table,
                                                           metadata_table)
      valid_values_table <- pxweb2_get_values(
        metadata_table,
        return_df_if_only_one_variable = FALSE,
        include_aggregations = aggregations_table,
        auto_limit = auto_limit,
        quiet = TRUE,
        base_url = base_url,
        lang = lang
      )

      query_table <- query_adjusted |>
        .pxweb2_harmonise_query_names(
          variables_df = variables_df_table,
          harmonise_variable_names = harmonise_variable_names
        )

      # only the variables and values that exist in this table
      for_table <- .pxweb2_query_for_table(query_table, variables_df_table, valid_values_table,
                                           latest_period_code = latest_period_code,
                                           eliminate = on_all_values_invalid != "*",
                                           allow_label_values = allow_label_values,
                                           allow_api_wildcards = allow_api_wildcards,
                                           codelist_ids = .pxweb2_codelist_ids(metadata_table))
      # as the query wrote them, before harmonise_variable_names
      as_written <- stats::setNames(names(query_adjusted), names(query_table))
      known_names <<- union(known_names, unname(as_written[for_table$known_names]))
      for (var in names(for_table$found)) {
        written <- as_written[[var]]
        known_values[[written]] <<- union(known_values[[written]], for_table$found[[var]])
      }

      # a required variable left out with NA cannot be called, whatever the mode
      saknas <- c(for_table$required_na, if (on_all_values_invalid != "*") for_table$no_match)
      if (length(saknas) > 0) {
        no_match_tables <<- c(no_match_tables, table_id)
        if (!quiet) message(
          "Table ", table_id, " is not fetched: it needs a value it has in ",
          paste(saknas, collapse = ", "), "."
        )
        return(NULL)
      }
      if (length(for_table$left_out) > 0 && !quiet) {
        message(
          "Table ", table_id, " is fetched without ", paste(for_table$left_out, collapse = ", "),
          ": it has none of the values asked for, so its rows cover all values of that variable."
        )
      }

      code_variables_table <- .pxweb2_harmonise_code_variables(
        code_variables,
        variables_df = variables_df_table,
        harmonise_variable_names = harmonise_variable_names
      )
      if (is.character(code_variables_table) && !"*" %in% code_variables_table) {
        known <- stringr::str_to_lower(c(variables_df_table$code, variables_df_table$label))
        code_variables_table <- code_variables_table[stringr::str_to_lower(code_variables_table) %in% known]
        if (length(code_variables_table) == 0) code_variables_table <- NULL
      }

      df_table <- pxr_pxweb2_get_data(
        table = metadata_table,
        query = for_table$query,
        lang = lang,
        output_format = output_format,
        base_url = base_url,
        on_all_values_invalid = on_all_values_invalid,
        allow_label_values = allow_label_values,
        strip_code_in_label = strip_code_in_label,
        latest_period_code = latest_period_code,
        allow_api_wildcards = allow_api_wildcards,
        deso_regso_versions = deso_regso_versions,
        split_deso_regso_by_municipality = split_deso_regso_by_municipality,
        include_aggregations = aggregations_table,
        auto_limit = auto_limit,
        code_variables = code_variables_table,
        quiet = quiet
      )

      # if the table returned NULL (e.g. due to "null" mode) -> skip harmonisation/mutate
      if (is.null(df_table)) return(NULL)

      df_table |>
        .pxweb2_harmonise_result_names(
          harmonise_variable_names = harmonise_variable_names
        ) |>
        dplyr::mutate(
          table_id = table_id,
          .before = 1
        )


    })

  names(result_list) <- tables_to_fetch

  result_list <- purrr::compact(result_list)

  .pxweb2_report_not_in_any_table(query_adjusted, known_names, known_values, quiet = quiet,
                                  latest_period_code = latest_period_code)

  if (length(result_list) == 0) {
    if (length(no_match_tables) > 0 && on_all_values_invalid == "stop") {
      stop(
        "None of the tables has the values asked for: ",
        paste(no_match_tables, collapse = ", "),
        ". The run is stopped because of 'on_all_values_invalid = \"stop\"'.",
        call. = FALSE
      )
    }
    return(NULL)
  }

  .pxweb2_warn_if_different_structure(result_list)

  dplyr::bind_rows(result_list)
} # end function .pxweb2_get_multiple_tables


# The part of a query that applies to one table: variables that the table lacks
# are left out, and so are values it lacks. If the table has none of the values of
# a variable it can eliminate, the variable is left out too (`left_out`). If it must
# have the variable, or the variable is a region, `no_match` names it and the table
# is not called: leaving out a region would give the whole country instead.
# With eliminate = FALSE every such variable goes to `no_match`. NA in a variable the
# table must have is `required_na`: the table cannot be called, whatever the mode.
# A query name is the code of a variable or, if no other name is that code, its label:
# in the same query, Region can be the code in one table and the label of another
# variable in the next. Values are matched by .pxweb2_match_values(). "9999" only
# means the latest period in the table's time variable; elsewhere it is an ordinary
# code. Wildcards and from(), to() and range() are resolved against the table's codes.
# `known_names`/`found` tell what the table had, as the query wrote it.
.pxweb2_query_for_table <- function(query, variables_df, valid_values_list,
                                    latest_period_code = NULL, eliminate = TRUE,
                                    allow_label_values = TRUE, allow_api_wildcards = TRUE,
                                    codelist_ids = NULL) {
  if (is.null(query) || .pxweb2_is_pxweb_query_list(query)) {
    return(list(query = query, no_match = character(), left_out = character(),
                required_na = character(), known_names = character(), found = list()))
  }
  exact <- match(tolower(names(query)), tolower(variables_df$code))
  by_label <- match(tolower(names(query)), tolower(variables_df$label))
  known_names <- names(query)[!is.na(exact) | !is.na(by_label)]
  by_label[by_label %in% exact] <- NA
  idx <- ifelse(is.na(exact), by_label, exact)
  out <- query[!is.na(idx)]
  codes <- variables_df$code[idx[!is.na(idx)]]
  no_match <- character()
  left_out <- character()
  required_na <- character()
  found <- list()
  can_leave_out <- variables_df$code[variables_df$elimination %in% TRUE & !variables_df$role %in% "geo"]
  required <- variables_df$code[!variables_df$elimination %in% TRUE]
  time_vars <- variables_df$code[variables_df$role %in% "time"]

  for (i in seq_along(out)) {
    val <- out[[i]]
    if (is.null(val) || (length(val) == 1 && is.na(val))) {
      out[i] <- list(NA)
      if (codes[i] %in% required) required_na <- c(required_na, codes[i])
      next
    }
    val <- as.character(val)
    is_agg <- length(val) == 1 && grepl("^agg_", val)
    if (is_agg && (is.null(codelist_ids) || val %in% codelist_ids)) next
    if (!is_agg && .pxweb2_is_special_value(val)) next
    ok <- valid_values_list[[codes[i]]]
    if (is.null(ok) || !"code" %in% names(ok)) next
    is_time <- codes[i] %in% time_vars
    # for each value what it means in this table, nothing if the table lacks it
    means <- lapply(val, function(v) {
      vals <- if (is_agg) character() else .pxweb2_expand_value(v, codes[i], valid_values_list, allow_api_wildcards)
      vals[.pxweb2_match_values(vals, ok, allow_label_values) | (is_time & vals %in% latest_period_code)]
    })
    hit <- lengths(means) > 0
    found[[names(out)[i]]] <- val[hit]
    if (any(hit)) {
      out[[i]] <- unique(unlist(means, use.names = FALSE))
    } else if (eliminate && codes[i] %in% can_leave_out) {
      out[[i]] <- NA
      left_out <- c(left_out, codes[i])
    } else {
      no_match <- c(no_match, codes[i])
    }
  }
  list(query = out, no_match = no_match, left_out = left_out, required_na = required_na,
       known_names = known_names, found = found)
}

# TRUE for the values that are a code or, unless labels are turned off, a label among
# ok (code, label). The same matching is used for the cell count in the app.
.pxweb2_match_values <- function(val, ok, allow_label_values = TRUE) {
  val %in% ok$code | (isTRUE(allow_label_values) & tolower(val) %in% tolower(ok$label))
}

# A wildcard or from(), to() or range() -> the codes it means; anything else as it is.
.pxweb2_expand_value <- function(v, var, valid_values_list, allow_api_wildcards = TRUE) {
  if (!grepl("[*?(]", v)) return(v)
  one <- list(selection = list(list(variableCode = var, valueCodes = list(v))))
  res <- .pxweb2_resolve_api_wildcards(one, valid_values_list, allow_api_wildcards)
  as.character(unlist(res$selection[[1]]$valueCodes, use.names = FALSE))
}

# The ids of the code lists a table offers
.pxweb2_codelist_ids <- function(metadata) {
  as.character(unlist(lapply(metadata$dimension, function(d) {
    lapply(d$extension$codelists, function(cl) cl$id)
  }), use.names = FALSE))
}

# include_aggregations with a named vector: only the variables and code lists this
# table has. The same indelning can have different ids in different tables, so a
# variable may be named once per id, e.g. c(Alder = "agg_a", Alder = "agg_b").
.pxweb2_aggregations_for_table <- function(include_aggregations, variables_df, metadata = NULL) {
  if (is.null(names(include_aggregations))) return(include_aggregations)
  known <- tolower(c(variables_df$code, variables_df$label))
  keep <- include_aggregations[tolower(names(include_aggregations)) %in% known]
  if (!is.null(metadata)) {
    keep <- keep[keep %in% c(.pxweb2_codelist_ids(metadata), "all", "codelists", "none") |
                   !grepl("^agg_|^vs_", keep)]
  }
  if (length(keep) == 0) "none" else keep
}

# What a query asks for that no table has: the variables give a warning, as
# for a single table; the values a message.
.pxweb2_report_not_in_any_table <- function(query, known_names, known_values, quiet = FALSE,
                                            latest_period_code = NULL) {
  if (is.null(query) || .pxweb2_is_pxweb_query_list(query)) return(invisible(NULL))
  unknown <- names(query)[!tolower(names(query)) %in% tolower(known_names)]
  if (length(unknown) > 0) {
    warning("Unknown variables in `query` are ignored: ", paste(unknown, collapse = ", "),
            call. = FALSE)
  }
  if (quiet) return(invisible(NULL))
  for (var in names(known_values)) {
    missing <- setdiff(as.character(query[[var]]), c(known_values[[var]], latest_period_code))
    if (length(missing) > 0) {
      message("Values of ", var, " that no table has: ", paste(missing, collapse = ", "), ".")
    }
  }
  invisible(NULL)
}


# helper: harmonise query names
# lets the user write Region = ... even if a given table actually has the variable Kommun.
.pxweb2_harmonise_query_names <- function(
    query,
    variables_df,
    harmonise_variable_names = NULL
) {

  if (is.null(query) || is.null(harmonise_variable_names)) {
    return(query)
  }

  if (.pxweb2_is_pxweb_query_list(query)) {
    return(query)
  }

  if (is.null(names(harmonise_variable_names))) {
    stop(
      "harmonise_variable_names must be a named vector, e.g. c(\"Region\" = \"Kommun\").",
      call. = FALSE
    )
  }

  table_variables <- variables_df$code
  table_variables_lc <- stringr::str_to_lower(table_variables)

  query_names <- names(query)
  query_names_lc <- stringr::str_to_lower(query_names)

  for (standard_name in names(harmonise_variable_names)) {

    alt_name <- unname(harmonise_variable_names[[standard_name]])

    standard_name_lc <- stringr::str_to_lower(standard_name)
    alt_name_lc <- stringr::str_to_lower(alt_name)

    query_standard_pos <- which(query_names_lc == standard_name_lc)
    table_standard_pos <- which(table_variables_lc == standard_name_lc)
    table_alt_pos <- which(table_variables_lc == alt_name_lc)

    if (
      length(query_standard_pos) > 0 &&
      length(table_standard_pos) == 0 &&
      length(table_alt_pos) > 0
    ) {
      names(query)[query_standard_pos] <- table_variables[table_alt_pos[[1]]]
    }
  }

  query
} # end function .pxweb2_harmonise_query_names


# Variables whose labels can start with the code: regions and those the metadata
# says show the code with the value. Other labels are left alone, since the
# number in "20 år" with the code 20 is part of the name.
.pxweb2_label_has_code <- function(code, label, role, show) {
  show %in% "code_value" | role %in% "geo" |
    stringr::str_to_lower(code) == "region" | stringr::str_to_lower(label) == "region"
}

.pxweb2_strip_code_in_label <- function(label, code) {
  label_chr <- as.character(label)
  code_chr <- as.character(code)

  # if the code column happens to contain "2082 Sater" instead of just "2082",
  # take the first token as the candidate code.
  code_candidate <- stringr::str_extract(code_chr, "^\\S+")

  # only strip if code_candidate actually looks code-like, i.e. contains a digit.
  # this prevents "Stockholms lan" from becoming "lan" in tables where the label is already clean.
  code_looks_like_code <- stringr::str_detect(code_candidate, "\\d")

  cleaned <- dplyr::if_else(
    is.na(label_chr) | is.na(code_candidate) | !code_looks_like_code,
    label_chr,
    stringr::str_remove(
      label_chr,
      paste0("^", stringr::str_escape(code_candidate), "\\s+")
    )
  )

  cleaned <- stringr::str_trim(cleaned)

  # if the label was only a code, e.g. "0114A0010", keep the original
  dplyr::if_else(
    is.na(cleaned) | cleaned == "",
    label_chr,
    cleaned
  )
}


# helper: harmonise result names
# renames result columns after fetching, e.g. Kommun -> Region and kommun_kod -> region_kod
.pxweb2_harmonise_result_names <- function(
    df,
    harmonise_variable_names = NULL
) {

  if (is.null(df) || is.null(harmonise_variable_names)) {
    return(df)
  }

  if (is.null(names(harmonise_variable_names))) {
    stop(
      "harmonise_variable_names must be a named vector, e.g. c(\"Region\" = \"Kommun\").",
      call. = FALSE
    )
  }

  for (standard_name in names(harmonise_variable_names)) {

    alt_name <- unname(harmonise_variable_names[[standard_name]])

    standard_name_lc <- stringr::str_to_lower(standard_name)
    alt_name_lc <- stringr::str_to_lower(alt_name)

    names_lc <- stringr::str_to_lower(names(df))

    # label column, e.g. kommun -> region
    alt_pos <- which(names_lc == alt_name_lc)
    standard_pos <- which(names_lc == standard_name_lc)

    if (length(alt_pos) > 0 && length(standard_pos) == 0) {
      names(df)[alt_pos] <- standard_name_lc
    }

    # code column, e.g. kommun_kod -> region_kod
    alt_code <- paste0(alt_name_lc, "_kod")
    standard_code <- paste0(standard_name_lc, "_kod")

    names_lc <- stringr::str_to_lower(names(df))
    
    alt_code_pos <- which(names_lc == alt_code)
    standard_code_pos <- which(names_lc == standard_code)

    if (length(alt_code_pos) > 0 && length(standard_code_pos) == 0) {
      names(df)[alt_code_pos] <- standard_code
    }
  }

  df
} # end function .pxweb2_harmonise_result_names

.pxweb2_resolve_api_wildcards <- function(query_list,
                                                valid_values_list,
                                                allow_api_wildcards = TRUE) {
  
  if (!isTRUE(allow_api_wildcards)) return(query_list)
  
  wildcard_to_regex <- function(x) {
    x <- stringr::str_replace_all(x, "([\\.\\+\\^\\$\\(\\)\\[\\]\\{\\}\\|\\\\])", "\\\\\\1")
    x <- stringr::str_replace_all(x, "\\*", ".*")
    x <- stringr::str_replace_all(x, "\\?", ".")
    paste0("^", x, "$")
  }
  
  query_list$selection <- purrr::map(query_list$selection, function(s) {
    
    var <- s$variableCode
    vals <- unlist(s$valueCodes, use.names = FALSE)
    
    if (length(vals) == 0 || is.null(vals)) return(s)
    
    ok_tbl <- valid_values_list[[var]]
    if (is.null(ok_tbl) || !"code" %in% names(ok_tbl)) return(s)
    
    if ("type" %in% names(ok_tbl)) {
      ok_tbl <- ok_tbl |>
        dplyr::filter(type == "Variable")
    }
    
    ok_codes <- ok_tbl$code
    
    vals_expanded <- purrr::map(vals, function(v) {
      
      # leave full wildcard, top/bottom and aggregation specials alone
      if (identical(v, "*") ||
          identical(v, "**") ||
          stringr::str_detect(v, "^\\s*(top|bottom)\\s*\\(\\s*\\d+\\s*\\)\\s*$") ||
          stringr::str_detect(v, "^agg_")) {
        return(v)
      }

      # from(code), to(code) and range(code1, code2) are resolved here against
      # the table's values (in table order), so that cells can be counted
      # and the request split like any other list of codes
      span <- stringr::str_match(
        v, "^\\s*(from|to|range)\\s*\\(\\s*([^,\\)]+?)\\s*(?:,\\s*([^\\)]+?)\\s*)?\\)\\s*$"
      )
      if (!is.na(span[1, 1])) {
        fn <- tolower(span[1, 2])
        a <- match(span[1, 3], ok_codes)
        b <- if (fn == "range") match(span[1, 4], ok_codes) else NA_integer_
        idx <- switch(fn,
          from  = if (!is.na(a)) seq(a, length(ok_codes)),
          to    = if (!is.na(a)) seq_len(a),
          range = if (!is.na(a) && !is.na(b)) seq(min(a, b), max(a, b))
        )
        return(if (length(idx) > 0) ok_codes[idx] else v)
      }

      # only expand if the value contains * or ?
      if (stringr::str_detect(v, "[\\*\\?]")) {
        pattern <- wildcard_to_regex(v)
        hits <- ok_codes[stringr::str_detect(ok_codes, pattern)]
        
        if (length(hits) == 0) {
          return(v)
        }
        
        return(hits)
      }
      
      v
    }) |>
      unlist(use.names = FALSE) |>
      unique()
    
    s$valueCodes <- as.list(vals_expanded)
    s
  })
  
  query_list
}


.pxweb2_resolve_latest_period <- function(query_list,
                                              variables_df,
                                              valid_values_list,
                                              latest_period_code = "9999") {
  
  if (is.null(latest_period_code)) return(query_list)
  
  time_vars <- variables_df |>
    dplyr::filter(role == "time") |>
    dplyr::pull(code)
  
  if (length(time_vars) == 0) return(query_list)
  
  query_list$selection <- purrr::map(query_list$selection, function(s) {
    
    var <- s$variableCode
    
    if (!var %in% time_vars) return(s)
    
    vals <- unlist(s$valueCodes, use.names = FALSE)
    
    if (!latest_period_code %in% vals) return(s)
    
    ok_tbl <- valid_values_list[[var]]
    
    if (is.null(ok_tbl) || !"code" %in% names(ok_tbl)) {
      stop("Cannot find valid time values for the variable: ", var)
    }

    # use only ordinary variable values, not aggregation rows
    if ("type" %in% names(ok_tbl)) {
      ok_tbl <- ok_tbl |>
        dplyr::filter(type == "Variable")
    }

    latest_value <- ok_tbl |>
      dplyr::pull(code) |>
      dplyr::last()

    if (is.na(latest_value) || length(latest_value) == 0) {
      stop("Cannot determine the latest time value for the variable: ", var)
    }

    vals <- dplyr::if_else(vals == latest_period_code, latest_value, vals)
    
    s$valueCodes <- as.list(vals)
    s
  })
  
  query_list
}

.pxweb2_sanitize_query_values <- function(query, valid_values_list, 
                                                on_all_values_invalid = "stop",
                                                warn = TRUE) {
  if (!on_all_values_invalid %in% c("stop", "*", "null")) {
    stop("Invalid value for on_all_values_invalid. Allowed: \"stop\", \"*\", \"null\".")
  }

  removed <- list()

  query$selection <- purrr::map(query$selection, function(s) {
    var  <- s$variableCode
    vals <- unlist(s$valueCodes, use.names = FALSE)

    # leave special expressions alone ("*", "**", top()/bottom(), "agg_...");
    # they are handled when the requests are expanded
    if (.pxweb2_is_special_value(vals)) {
      return(s)
    }

    ok_tbl <- valid_values_list[[var]]
    if (is.null(ok_tbl) || !"code" %in% names(ok_tbl)) return(s)

    ok <- ok_tbl$code
    keep <- vals[vals %in% ok]
    bad  <- setdiff(vals, keep)

    if (length(bad) > 0) removed[[var]] <<- unique(c(removed[[var]], bad))

    # if everything was invalid: stop or replace with "*"
    if (length(keep) == 0) {

      if (on_all_values_invalid == "stop") {
        stop(
          "Invalid values in the query for the variable '", var, "': ",
          paste(vals, collapse = ", "),
          ". The run is stopped because of ",
          "'on_all_values_invalid = \"stop\"'."
        )
      }

      if (on_all_values_invalid == "*") {
        if (warn) {
          cat(
            "Invalid values in the query for the variable '", var,
            "'; all values are included because ",
            "'on_all_values_invalid = \"*\"'.\n",
            sep = ""
          )
        }
        s$valueCodes <- list("*")
        return(s)
      }

      if (on_all_values_invalid == "null") {
        if (warn) {
          cat(
            "Invalid values in the query for the variable '", var,
            "'; the variable is dropped because ",
            "'on_all_values_invalid = \"null\"'.\n",
            sep = ""
          )
        }
        s$valueCodes <- NULL
        return(s)
      }
    }

    s$valueCodes <- as.list(keep)
    s
  })

  if (warn && length(removed) > 0) {
    msg <- paste(
      purrr::imap_chr(removed, ~ paste0(.y, ": ", paste(.x, collapse = ", "))),
      collapse = " | "
    )
    cat(paste0("The following values do not exist in the table and were therefore removed:\n", msg, "\n"))
  }

  return(query)
}


.pxweb2_rate_state <- new.env(parent = emptyenv())
.pxweb2_rate_state$times <- numeric(0)   # timestamps (sec) of recent calls
.pxweb2_rate_state$max_calls <- 30L      # replaced by the API's /config when it has been read
.pxweb2_rate_state$window <- 10          # seconds

.pxweb2_set_rate_limit <- function(max_calls, window) {
  .pxweb2_rate_state$max_calls <- as.integer(max_calls)
  .pxweb2_rate_state$window <- as.numeric(window)
  invisible(NULL)
}

.pxweb2_rate_limiter <- function() {
  st <- .pxweb2_rate_state
  safety <- 0.05                    # small margin (50 ms)
  now <- as.numeric(Sys.time())

  # discard calls older than the window
  st$times <- st$times[now - st$times < st$window]

  # if we already have max_calls in the window: wait until the first falls out
  if (length(st$times) >= st$max_calls) {
    wait <- st$window - (now - st$times[1]) + safety
    if (wait > 0) Sys.sleep(wait)
    now <- as.numeric(Sys.time())
    st$times <- st$times[now - st$times < st$window]
  }

  # register that we are making a call now
  st$times <- c(st$times, now)
  invisible(NULL)
}


# session cache for metadata and /config, keyed by URL. Entries older than
# getOption("pxweb2r.cache_minutes", 30) minutes are fetched again.
.pxweb2_cache <- new.env(parent = emptyenv())

.pxweb2_cache_get <- function(key) {
  entry <- .pxweb2_cache[[key]]
  if (is.null(entry)) return(NULL)
  max_age <- getOption("pxweb2r.cache_minutes", 30) * 60
  if (as.numeric(Sys.time()) - entry$time > max_age) return(NULL)
  entry$value
}

.pxweb2_cache_set <- function(key, value) {
  assign(key, list(time = as.numeric(Sys.time()), value = value), envir = .pxweb2_cache)
  invisible(value)
}

#' Clear the session cache
#'
#' `pxweb2r` keeps table metadata and the API configuration in memory for
#' `getOption("pxweb2r.cache_minutes", 30)` minutes, so that repeated calls
#' against the same table do not fetch the metadata again. This clears it.
#'
#' @return `NULL`, invisibly.
#' @noRd
pxweb2_clear_cache <- function() {
  rm(list = ls(.pxweb2_cache, all.names = TRUE), envir = .pxweb2_cache)
  invisible(NULL)
}

#' Get the API's limits
#'
#' Reads the `/config` endpoint of a PxWeb API v2, or `?config` in API v1: max
#' number of data cells per
#' call and how many calls are allowed per time window. `pxr_pxweb2_get_data()`
#' uses these to size its chunks and to pace its calls. If the endpoint cannot
#' be read, Statistics Sweden's limits are used (150 000 cells, 30 calls per
#' 10 seconds).
#'
#' @param base_url Base URL of the PxWeb API (the tables endpoint in v2).
#'
#' @return A list with `maxDataCells`, `maxCallsPerTimeWindow` and `timeWindow`
#'   (seconds), plus the other fields the API returns.
#' @noRd
pxweb2_get_config <- function(base_url = .pxweb2_default_base_url()) {
  .pxweb2_get_config(base_url)
}

.pxweb2_get_config <- function(base_url) {
  base_url <- .pxweb2_normalise_base_url(base_url)
  config_url <- paste0(.pxweb2_api_root(base_url), "config")

  config <- .pxweb2_cache_get(config_url)

  if (is.null(config)) {
    config <- tryCatch({
      if (.dst_is_url(base_url)) {
        .dst_read_config()
      } else if (.pxweb1_is_url(base_url)) {
        .pxweb1_read_config(base_url)
      } else {
        resp <- .pxweb2_GET(config_url, httr::accept_json())
        if (httr::status_code(resp) == 200) {
          httr::content(resp, as = "parsed", encoding = "UTF-8")
        } else NULL
      }
    }, error = function(e) NULL)

    config <- .pxweb2_normalise_config(config)
    .pxweb2_cache_set(config_url, config)
  }

  .pxweb2_set_rate_limit(config$maxCallsPerTimeWindow, config$timeWindow)
  config
}

# fill in missing or invalid limits with Statistics Sweden's values
.pxweb2_normalise_config <- function(config) {
  defaults <- list(maxDataCells = 150000, maxCallsPerTimeWindow = 30L, timeWindow = 10)
  if (!is.list(config)) config <- list()
  for (nm in names(defaults)) {
    v <- suppressWarnings(as.numeric(config[[nm]] %||% NA)[1])
    if (is.na(v) || v < 1) v <- defaults[[nm]]
    config[[nm]] <- v
  }
  config$maxCallsPerTimeWindow <- as.integer(config$maxCallsPerTimeWindow)
  config
}


# parses a json-stat2 response once and returns both a label data frame (the
# same as rjstat::fromJSONstat(naming = "label")) and the codes for code_dims.
# Rows are in json-stat order (the last dimension varies fastest). Falls back to
# rjstat for anything unusual, e.g. sparse values given as an object. JSON-stat 1
# (Statistics Denmark) has the dataset one level down, and id and size in dimension.
.pxweb2_parse_jsonstat2 <- function(json_text, code_dims = character()) {

  fallback <- function() {
    list(
      labels = rjstat::fromJSONstat(json_text, naming = "label"),
      codes = if (length(code_dims) > 0) rjstat::fromJSONstat(json_text, naming = "id") else NULL
    )
  }

  js <- tryCatch(
    jsonlite::fromJSON(json_text, simplifyVector = TRUE, simplifyDataFrame = FALSE,
                       simplifyMatrix = FALSE),
    error = function(e) NULL
  )
  if (!is.null(js$dataset)) {
    js <- js$dataset
    js$id <- js$dimension$id
    js$size <- js$dimension$size
  }
  if (is.null(js) || is.list(js$value) || is.null(js$id) || is.null(js$size)) {
    return(fallback())
  }

  ids <- as.character(js$id)
  sizes <- as.integer(js$size)
  n <- prod(sizes)
  values <- js$value
  if (is.null(values)) values <- rep(NA_real_, n)
  values <- suppressWarnings(as.numeric(values))
  if (length(values) != n) return(fallback())

  labels <- list()
  codes <- list()
  dim_labels <- stats::setNames(character(length(ids)), ids)
  for (k in seq_along(ids)) {
    dim <- js$dimension[[ids[k]]]
    index <- dim$category$index
    dim_codes <- if (is.null(index)) {
      names(dim$category$label)
    } else if (is.list(index)) {
      names(index)[order(unlist(index))]
    } else {
      as.character(index)
    }
    if (length(dim_codes) != sizes[k]) return(fallback())

    lab <- dim$category$label
    dim_values <- if (is.null(lab)) dim_codes else {
      l <- unlist(lab)[dim_codes]
      ifelse(is.na(l), dim_codes, l)
    }

    # position of each row in this dimension
    each <- prod(sizes[-seq_len(k)])
    pos <- rep(rep(seq_len(sizes[k]), each = each), length.out = n)

    dim_labels[[ids[k]]] <- dim$label %||% ids[k]
    labels[[dim_labels[[ids[k]]]]] <- unname(dim_values[pos])
    if (ids[k] %in% code_dims) codes[[ids[k]]] <- dim_codes[pos]
  }

  labels$value <- values
  list(
    dim_labels = dim_labels,
    labels = as.data.frame(labels, stringsAsFactors = FALSE, check.names = FALSE),
    codes = if (length(codes) > 0) as.data.frame(codes, stringsAsFactors = FALSE, check.names = FALSE) else NULL
  )
}


.pxweb2_api_log <- new.env(parent = emptyenv())

.pxweb2_api_log$rows <- list()

.pxweb2_api_log_reset <- function() {
  .pxweb2_api_log$rows <- list()
  invisible(NULL)
}

.pxweb2_api_log_get <- function() {
  if (length(.pxweb2_api_log$rows) == 0) {
    return(tibble::tibble(
      time = as.POSIXct(character()),
      method = character(),
      url = character(),
      status = integer(),
      attempt = integer(),
      endpoint_type = character()
    ))
  }
  tibble::as_tibble(do.call(rbind, lapply(.pxweb2_api_log$rows, as.data.frame)))
}

.pxweb2_api_endpoint_type <- function(url) {
  url_chr <- as.character(url)
  
  if (grepl("/metadata", url_chr, fixed = TRUE)) {
    return("metadata")
  }
  
  if (grepl("/data", url_chr, fixed = TRUE)) {
    return("data")
  }
  
  if (grepl("/tables", url_chr, fixed = TRUE)) {
    return("tables")
  }
  
  if (grepl("/codelists", url_chr, fixed = TRUE) ||
      grepl("codelist", url_chr, ignore.case = TRUE)) {
    return("codelist")
  }

  "other"
}

.pxweb2_api_log_add <- function(method, url, status, attempt) {
  .pxweb2_api_log$rows <- c(
    .pxweb2_api_log$rows,
    list(list(
      time = Sys.time(),
      method = as.character(method),
      url = as.character(url),
      status = as.integer(status),
      attempt = as.integer(attempt),
      endpoint_type = .pxweb2_api_endpoint_type(url)
    ))
  )
  
  invisible(NULL)
}

.pxweb2_GET <- function(url, ..., max_tries = 3, retry_wait_default = 10) {
  
  resp <- NULL
  
  for (attempt in seq_len(max_tries)) {
    
    .pxweb2_rate_limiter()
    
    resp <- httr::GET(url, ...)
    
    .pxweb2_api_log_add(
      method = "GET",
      url = url,
      status = httr::status_code(resp),
      attempt = attempt
    )
    
    if (httr::status_code(resp) != 429) {
      return(resp)
    }
    
    retry_after <- suppressWarnings(as.numeric(httr::headers(resp)[["retry-after"]]))
    wait <- if (length(retry_after) == 1 && !is.na(retry_after)) retry_after else retry_wait_default
    Sys.sleep(wait + 0.2)
  }
  
  resp
}

.pxweb2_POST <- function(url, ..., max_tries = 3, retry_wait_default = 10) {
  
  resp <- NULL
  
  for (attempt in seq_len(max_tries)) {
    
    .pxweb2_rate_limiter()
    
    resp <- httr::POST(url, ...)
    
    .pxweb2_api_log_add(
      method = "POST",
      url = url,
      status = httr::status_code(resp),
      attempt = attempt
    )
    
    if (httr::status_code(resp) != 429) {
      return(resp)
    }
    
    retry_after <- suppressWarnings(as.numeric(httr::headers(resp)[["retry-after"]]))
    wait <- if (length(retry_after) == 1 && !is.na(retry_after)) retry_after else retry_wait_default
    Sys.sleep(wait + 0.2)
  }
  
  resp
}


.pxweb2_make_chunks <- function(variables_df, query, valid_values_list,
                                      max_cells = 150000, exclude = character()) {
  total_cells <- .pxweb2_count_cells(variables_df, query)
  
  if (total_cells <= max_cells) {
    return(list(query))
  }
  
  split_var <- .pxweb2_choose_split_variable(variables_df, query, exclude = exclude)

  if (is.null(split_var)) {
    sizes <- sort(.pxweb2_selection_sizes(variables_df, query), decreasing = TRUE)
    stop(
      "Too many cells selected: ", format(total_cells, big.mark = " "),
      " cells remain per request even after splitting on every variable (max_cells = ",
      format(max_cells, big.mark = " "), "). Restrict the query further ",
      "(current number of selected values):\n",
      paste0("  ", names(sizes), ": ", format(sizes, big.mark = " "), collapse = "\n"),
      call. = FALSE
    )
  }
  
  selected_vals <- .pxweb2_get_valuecodes(query, split_var)
  
  if (is.null(selected_vals) || length(selected_vals) == 0 ||
      (length(selected_vals) == 1 && identical(selected_vals, "*"))) {
    
    ok_tbl <- valid_values_list[[split_var]]

    if (is.null(ok_tbl)) {
      stop("Cannot split the request on '", split_var, "': its valid values are unknown.",
           call. = FALSE)
    }
    
    if ("type" %in% names(ok_tbl)) {
      ok_tbl <- ok_tbl |>
        dplyr::filter(type == "Variable")
    }
    
    all_vals <- ok_tbl |>
      dplyr::pull(code)
    
  } else {

    all_vals <- selected_vals
  }

  # If even a single value of split_var leaves the request too large, split
  # on split_var value by value and split each part further on the next
  # variable.
  min_possible_cost <- .pxweb2_count_cells(
    variables_df,
    .pxweb2_set_valuecodes_in_query(query, split_var, all_vals[1])
  )

  if (min_possible_cost > max_cells) {
    chunks <- lapply(all_vals, function(v) {
      .pxweb2_make_chunks(
        variables_df,
        .pxweb2_set_valuecodes_in_query(query, split_var, v),
        valid_values_list = valid_values_list,
        max_cells = max_cells,
        exclude = c(exclude, split_var)
      )
    })
    return(purrr::flatten(chunks))
  }

  # greedy packing (purrr::accumulate): fill as close to max_cells as possible
  state <- purrr::accumulate(
    all_vals,
    .init = list(
      cur_vals = character(),
      cur_cells = 0L,
      chunks = list()
    ),
    .f = function(st, v) {
      # the cost of adding v (incl. other dimensions)
      cost <- .pxweb2_count_cells(
        variables_df,
        .pxweb2_set_valuecodes_in_query(query, split_var, v)
      )

      # if v on its own is larger than max_cells (unlikely, but guard for it)
      if (cost > max_cells) {
        if (length(st$cur_vals) > 0) {
          st$chunks <- append(st$chunks, list(st$cur_vals))
          st$cur_vals <- character()
          st$cur_cells <- 0L
        }
        st$chunks <- append(st$chunks, list(v))
        return(st)
      }
      
      # if it does not fit in the current chunk: close the chunk and start a new one
      if (st$cur_cells + cost > max_cells && length(st$cur_vals) > 0) {
        st$chunks <- append(st$chunks, list(st$cur_vals))
        st$cur_vals <- v
        st$cur_cells <- cost
        return(st)
      }

      # otherwise add to the current chunk
      st$cur_vals <- c(st$cur_vals, v)
      st$cur_cells <- st$cur_cells + cost
      st
    }
  ) 
  
  state <- state[[length(state)]]
  
  # add the last chunk if there is one
  if (length(state$cur_vals) > 0) {
    state$chunks <- append(state$chunks, list(state$cur_vals))
  }
  
  purrr::map(state$chunks, ~ .pxweb2_set_valuecodes_in_query(query, split_var, .x))
}


.pxweb2_set_valuecodes_in_query <- function(query, variable, values) {
  idx <- purrr::detect_index(
    query$selection,
    ~ identical(.x$variableCode, variable)
  )
  
  if (idx == 0L) {
    stop("Variable missing from query: ", variable)
  }
  
  query$selection[[idx]]$valueCodes <- as.list(values)
  query
}


.pxweb2_choose_split_variable <- function(variables_df, query = list(), exclude = character()) {
  # choose a variable to split on when a request has more cells than the API allows.
  # Variables in `exclude` (already split) and variables with only one selected
  # value are skipped. Returns NULL when nothing is left to split on.

  selected <- .pxweb2_selection_sizes(variables_df, query)

  candidates <- variables_df |>
    dplyr::mutate(selected = unname(selected[code])) |>
    dplyr::filter(!code %in% exclude, selected > 1)

  if (nrow(candidates) == 0) {
    return(NULL)
  }

  # prioritise geo, then other variables by size; time and contents only as a last resort
  candidates |>
    dplyr::mutate(priority = dplyr::case_when(
      role == "geo" ~ 3,
      role %in% c("time", "contents") ~ 1,
      TRUE ~ 2
    )) |>
    dplyr::arrange(dplyr::desc(priority), dplyr::desc(size)) |>
    dplyr::slice(1) |>
    dplyr::pull(code)
}

.pxweb2_list_to_query_list <- function(variables_df,
                                             query = list(),
                                             valid_values_list, 
                                             default_value = "*",
                                             allow_label_values = TRUE,
                                             warn = TRUE) {
  stopifnot(is.data.frame(variables_df))
  
  if (!all(c("code", "label", "elimination") %in% names(variables_df))) {
    stop("variables_df must at least have the columns: code, label, elimination")
  }

  # match exactly on code (fuzzy matching could be added later if wanted)
  valid_codes <- variables_df$code
  elim_map <- stats::setNames(as.logical(variables_df$elimination), variables_df$code)

  # allow both code and label (case-insensitive) in query names ---
  key_to_code <- c(
    stats::setNames(variables_df$code, tolower(variables_df$code)),
    stats::setNames(variables_df$code, tolower(variables_df$label))
  )

  # normalise the query: map names (code/label) -> code
  q_names <- names(query)
  q_keys  <- tolower(q_names)

  mapped_codes <- unname(key_to_code[q_keys])

  # unknown keys = those that could not be mapped
  unknown <- q_names[is.na(mapped_codes)]
  
  # build a normalised query keyed by code
  query_norm <- query[!is.na(mapped_codes)]
  names(query_norm) <- mapped_codes[!is.na(mapped_codes)]

  # --- allow labels as valueCodes (only if they are not already valid codes) ---
  if (isTRUE(allow_label_values) && !is.null(valid_values_list) && length(query_norm) > 0) {
    query_norm <- purrr::imap(query_norm, function(val, var) {
      # leave special cases alone
      if (is.null(val) || (length(val) == 1 && (is.na(val) || identical(val, "*"))) ||
          (is.character(val) && length(val) == 1 &&
           grepl("^\\s*(top|bottom)\\s*\\(\\s*\\d+\\s*\\)\\s*$", val, ignore.case = TRUE))) {
        return(val)
      }
      
      ok_tbl <- valid_values_list[[var]]
      if (is.null(ok_tbl) || !all(c("code", "label") %in% names(ok_tbl))) return(val)
      
      codes <- ok_tbl$code
      lbl_map <- stats::setNames(ok_tbl$code, tolower(ok_tbl$label))
      
      v <- as.character(val)
      v2 <- purrr::map_chr(v, function(x) {
        #if (x %in% codes) x else (lbl_map[[tolower(x)]] %||% x)
        if (x %in% codes) x else {
          hit <- unname(lbl_map[tolower(x)])
          if (length(hit) == 0 || is.na(hit) || identical(hit, character(0))) x else hit
        }
        
      })
      v2 <- unique(v2)
      return(v2)
    })
  }
  
  
  # warn if the same code was given more than once (via both code and label)
  dup_codes <- names(query_norm)[duplicated(names(query_norm))]
  if (warn && length(dup_codes) > 0) {
    warning("The same variable was given more than once (via code/label). Last wins: ",
            paste(unique(dup_codes), collapse = ", "))
  }
  query_norm <- query_norm[!duplicated(names(query_norm), fromLast = TRUE)]

  # if the user passes unknown variables -> warn but ignore
  if (warn && length(unknown) > 0) {
    warning("Unknown variables in `query` are ignored: ", paste(unknown, collapse = ", "))
  }

  # build the selection, but:
  # - default "*" for every variable that exists in the table
  # - if query[[var]] is NA -> drop the variable (if eliminable; otherwise warn)
  selection <- purrr::map(valid_codes, function(var) {
    val <- if (var %in% names(query_norm)) query_norm[[var]] else default_value

    # NA => drop (i.e. return NULL so we can purrr::compact() later)
    if (length(val) == 1 && is.na(val)) {
      if (warn && !isTRUE(elim_map[[var]])) {
        warning(
          "The variable `", var, "` was set to NA (dropped), but is not eliminable according to the metadata. ",
          "The API may still require a selection for this dimension."
        )
      }
      return(NULL)
    }

    # allow the user to write "*" or TOP(1)/Top(1)/top(1) etc.
    # valueCodes must always become a list(...) for JSON
    if (identical(val, "*")) {
      vc <- list("*")
    } else if (is.character(val) && length(val) == 1) {
      vc <- list(val)
    } else {
      # vector (e.g. c("20","21","17")) -> list("20","21","17")
      vc <- as.list(val)
    }
    
    list(
      variableCode = var,
      valueCodes = vc
    )
  }) |>
    purrr::compact()
  
  list(selection = selection)
} 


.pxweb2_create_variable_query_list <- function(variables_df,
                                                     default_value = "*",
                                                     overrides = list()) {
  selections <- purrr::map(
    variables_df$code,
    function(var) {
      v <- if (!is.null(overrides[[var]])) overrides[[var]] else default_value
      
      list(
        variableCode = var,
        valueCodes = if (length(v) == 1 && is.na(v)) {
          list()                       # empty = removed in the next step
        } else if (identical(v, "*") || (is.character(v) && length(v) == 1)) {
          list(v)                      # e.g. "*", "top(4)"
        } else {
          as.list(as.character(v))     # e.g. c("20","21","17") -> list("20","21","17")
        }
      )
    }
  ) |>
    purrr::keep(~ length(.x$valueCodes) > 0)  # drop the ones that became NA
  
  list(selection = selections)
}



#' Get metadata for a PxWeb table
#'
#' @param table_id Table id, e.g. `"TAB6104"`. Obligatorisk.
#' @param base_url Provider id or base URL, see `pxweb2_base_url()`.
#' @param lang Language code; `NULL` = `getOption("pxweb2r.lang")` or the
#'   provider's default language.
#' @param use_cache If `TRUE` (default), metadata fetched earlier in the session
#'   is reused for `getOption("pxweb2r.cache_minutes", 30)` minutes. See
#'   `pxweb2_clear_cache()`.
#'
#' @return Metadata as a parsed list. An error is thrown if the table does not
#'   exist (HTTP 404) on the given `base_url`.
#' @noRd
pxweb2_get_metadata <- function(
    table_id,
    base_url = .pxweb2_default_base_url(),
    use_cache = TRUE,
    lang = NULL
){
  if (is.null(table_id)) stop("table_id must be supplied")
  base_url <- .pxweb2_normalise_base_url(base_url)
  .pxweb2_check_table_id(table_id, base_url)
  lang <- .pxweb2_resolve_lang(lang, base_url)

  if (.dst_is_url(base_url)) return(.dst_get_metadata(table_id, lang, use_cache))
  if (.pxweb1_is_url(base_url)) {
    return(.pxweb1_get_metadata(table_id, base_url, lang, use_cache))
  }

  meta_url <- paste0(base_url, table_id, "/metadata")
  cache_key <- paste0(meta_url, "?lang=", lang)

  if (isTRUE(use_cache)) {
    cached <- .pxweb2_cache_get(cache_key)
    if (!is.null(cached)) return(cached)
  }

  resp <- .pxweb2_GET(meta_url, httr::accept_json(), query = list(lang = lang))

  if (httr::status_code(resp) == 404) {
    stop(
      "Table id '", table_id, "' was not found on ", base_url,
      " (HTTP 404). Check the table id or base_url.",
      call. = FALSE
    )
  }
  httr::stop_for_status(resp)

  meta <- httr::content(resp, as = "parsed", encoding = "UTF-8")

  .pxweb2_cache_set(cache_key, meta)

  return(meta)
}

#' Check whether a table id exists
#'
#' Quick check against the metadata endpoint on the given `base_url`. Errors
#' other than HTTP 404 (network, 500 ...) are re-thrown, since they do not mean
#' the table is missing.
#'
#' @param table_id Table id to check. Obligatorisk.
#' @param base_url Base URL of the PxWeb API v2.
#'
#' @return `TRUE` if the table exists, `FALSE` otherwise.
#' @noRd
pxweb2_table_exists <- function(
    table_id,
    base_url = .pxweb2_default_base_url()
) {
  base_url <- .pxweb2_normalise_base_url(base_url)
  .pxweb2_check_table_id(table_id, base_url)

  is_v1 <- .pxweb1_is_url(base_url)
  is_dst <- .dst_is_url(base_url)
  meta_url <- if (is_v1) {
    .pxweb1_url(base_url, path = table_id)
  } else if (is_dst) {
    paste0(base_url, "tableinfo/", table_id, "?format=JSON")
  } else {
    paste0(base_url, table_id, "/metadata")
  }
  resp <- .pxweb2_GET(meta_url, httr::accept_json())

  if (httr::status_code(resp) == 404 || ((is_v1 || is_dst) && httr::status_code(resp) == 400)) return(FALSE)
  httr::stop_for_status(resp)
  TRUE
}

# derive the API root ("https://.../api/v2/") from a tables base_url
.pxweb2_api_root <- function(base_url) {
  .pxweb2_api_root_from_url(.pxweb2_normalise_base_url(base_url))
}

#' Get a single code list
#'
#' Fetches one code list (value set or aggregation) by id from the PxWeb API v2.
#' Code-list ids are global and do not require a table - e.g.
#' `"vs_RegionKommun07"` (municipalities), `"vs_RegionLän07"` (counties) or
#' `"agg_RegionLA2018"` (local labour-market areas). Use
#' `pxweb2_list_codelists()` to discover which ids a given table offers.
#'
#' @param codelist_id Code-list id, e.g. `"vs_RegionKommun07"`. Obligatorisk.
#' @param lang Language for labels, e.g. `"sv"` or `"en"`; `NULL` = the
#'   provider's default language.
#' @param base_url Provider id or base URL (see `pxweb2_base_url()`); the code
#'   lists endpoint is derived from it.
#'
#' @return A `tibble` with one row per value: `code`, `label`, and `value_map`
#'   (a list column - the member codes, which for a value set is just the code
#'   itself and for an aggregation is the set of aggregated codes).
#' @noRd
pxweb2_get_codelist <- function(
    codelist_id,
    lang = NULL,
    base_url = .pxweb2_default_base_url()
) {
  base_url <- .pxweb2_normalise_base_url(base_url)
  if (.pxweb1_is_url(base_url)) {
    stop("PxWeb API v1 has no code lists (", base_url, ").", call. = FALSE)
  }
  if (.dst_is_url(base_url)) stop("Statistics Denmark's API has no code lists.", call. = FALSE)
  lang <- .pxweb2_resolve_lang(lang, base_url)
  if (is.null(codelist_id) || length(codelist_id) != 1 || !is.character(codelist_id) ||
      is.na(codelist_id) || !nzchar(trimws(codelist_id))) {
    stop("codelist_id must be a non-empty string of length 1.", call. = FALSE)
  }

  cl_url <- paste0(.pxweb2_api_root(base_url), "codelists/", codelist_id)
  resp <- .pxweb2_GET(cl_url, httr::accept_json(), query = list(lang = lang))

  if (httr::http_error(resp)) {
    stop(
      "Could not fetch code list '", codelist_id, "' from ",
      .pxweb2_api_root(base_url), "codelists/ (HTTP ",
      httr::status_code(resp), "). Check the code-list id.",
      call. = FALSE
    )
  }

  cl <- httr::content(resp, as = "parsed", encoding = "UTF-8")
  values <- purrr::pluck(cl, "values", .default = list())

  if (length(values) == 0) {
    return(tibble::tibble(
      code = character(), label = character(), value_map = list()
    ))
  }

  tibble::tibble(
    code = trimws(purrr::map_chr(values, ~ .x$code %||% NA_character_)),
    label = trimws(purrr::map_chr(values, ~ .x$label %||% NA_character_)),
    value_map = purrr::map(values, ~ as.character(.x$valueMap %||% .x$code))
  )
}

#' List the code lists available for a table
#'
#' Reads a table's metadata and returns every code list (value sets and
#' aggregations) offered by its variables. There is no global code-list
#' listing in the API - they are only discoverable per table.
#'
#' @param table Table id or a metadata object from `pxweb2_get_metadata()`. Obligatorisk.
#' @param base_url Provider id or base URL, see `pxweb2_base_url()`.
#' @param lang Language code, used when `table` is a table id.
#'
#' @return A `tibble` with one row per code list: `variable` (the variable it
#'   belongs to), `id`, `type` (`"Valueset"` or `"Aggregation"`) and `label`.
#'   Fetch a specific one with `pxweb2_get_codelist()`.
#' @noRd
pxweb2_list_codelists <- function(
    table,
    base_url = .pxweb2_default_base_url(),
    lang = NULL
) {
  if (is.null(table)) stop("table must be supplied, either as a table id or a metadata object.")
  if (!is.list(table)) {
    metadata <- pxweb2_get_metadata(table, base_url = base_url, lang = lang)
  } else metadata <- table

  empty <- tibble::tibble(
    variable = character(), id = character(),
    type = character(), label = character()
  )

  rows <- purrr::imap(metadata$dimension, function(dim_el, dim_name) {
    cl <- purrr::pluck(dim_el, "extension", "codelists", .default = NULL)
    if (is.null(cl) || length(cl) == 0) return(empty)
    purrr::map_dfr(cl, function(item) {
      lbl <- purrr::pluck(item, "label", .default = NA_character_)
      if (is.list(lbl)) {
        lbl <- purrr::pluck(lbl, lang %||% "sv",
                            .default = (unlist(lbl, use.names = FALSE)[1] %||% NA_character_))
      }
      tibble::tibble(
        variable = dim_name,
        id       = purrr::pluck(item, "id",   .default = NA_character_),
        type     = purrr::pluck(item, "type", .default = NA_character_),
        label    = lbl %||% NA_character_
      )
    })
  })

  dplyr::bind_rows(rows)
}

.pxweb2_is_pxweb_query_list <- function(x) {
  is.list(x) &&
    !is.null(x$selection) &&
    is.list(x$selection) &&
    all(purrr::map_lgl(
      x$selection,
      ~ is.list(.x) && !is.null(.x$variableCode) && !is.null(.x$valueCodes)
    ))
}

# Number of selected cells per variable (named integer vector). Shared by
# .pxweb2_count_cells() (which takes the product) and the "too many cells"
# error message in .pxweb2_make_chunks() (which shows the per-variable
# breakdown so the user knows what to restrict).
.pxweb2_selection_sizes <- function(variables_df, query = list()) {

  if (length(query) == 0) {
    sizes <- as.integer(variables_df$size)
    return(stats::setNames(sizes, variables_df$code))
  }

  if (!.pxweb2_is_pxweb_query_list(query)) {
    stop("`query` must be a PxWeb query_list: list(selection = list(list(variableCode=..., valueCodes=list(...)), ...))")
  }

  if (!is.data.frame(variables_df) | !all(c("code", "label") %in% names(variables_df))) {
    stop("variables_df must be a data frame with the table's variables, obtainable with pxweb2_get_variables().")
  }

  # named map: selection_map[["Region"]] = c("20","21"), etc.
  selection_map <- purrr::map(
    query$selection,
    ~{
      var  <- .x$variableCode
      vals <- unlist(.x$valueCodes, use.names = FALSE)
      stats::setNames(list(vals), var)
    }
  ) |>
    purrr::flatten()

  sizes <- purrr::map_int(variables_df$code, function(id) {

    # if the variable is in the query but is NA => "deselected" => cell contribution = 1
    if (!is.null(selection_map[[id]]) &&
        length(selection_map[[id]]) == 1 &&
        is.na(selection_map[[id]][1])) {
      return(1L)
    }

    # if the variable is in the query with values
    if (!is.null(selection_map[[id]])) {
      vals <- selection_map[[id]]

      # wildcard => full size for that dimension
      if (length(vals) == 1 && identical(vals, "*")) {
        return(as.integer(variables_df$size[variables_df$code == id][1]))
      }

      # top(n)/bottom(n)
      if (length(vals) == 1 &&
          is.character(vals) &&
          grepl("^\\s*(top|bottom)\\s*\\(\\s*\\d+\\s*\\)\\s*$", vals, ignore.case = TRUE)) {
        n <- as.integer(gsub(".*\\(\\s*(\\d+)\\s*\\).*", "\\1", vals, perl = TRUE))
        return(n)
      }

      # explicit codes
      return(length(vals))
    }

    # if the variable is missing from the query => it is eliminated (a dropped
    # variable is not in the request), cell contribution = 1
    1L
  })

  stats::setNames(sizes, variables_df$code)
}

.pxweb2_count_cells <- function(variables_df, query = list()) {
  prod(.pxweb2_selection_sizes(variables_df, query))
}


#' Get the variables of a PxWeb table
#'
#' @param table Table id or a metadata object from `pxweb2_get_metadata()`. Obligatorisk.
#' @param base_url Provider id or base URL, see `pxweb2_base_url()`.
#' @param lang Language code, used when `table` is a table id.
#'
#' @return A `tibble` with one row per variable (code, label, number of values,
#'   elimination, role, etc.).
#' @noRd
pxweb2_get_variables <- function(
    table,             # a table id or a metadata object
    base_url = .pxweb2_default_base_url(),
    lang = NULL               # language code, used when table is a table id
) {
  if (is.null(table)) stop("table must be supplied, either as a table id or a metadata object.")

  if (!is.list(table)) {
    metadata <- pxweb2_get_metadata(table, base_url = base_url, lang = lang)
  } else metadata <- table

  # extract all variables with both code and label, plus how many unique values they have
  variables <- tibble::tibble(
    code       = names(metadata$dimension),
    label      = purrr::map_chr(metadata$dimension, ~ .x$label %||% NA_character_),
    size = purrr::map_int(metadata$dimension, ~ length(.x$category$label %||% list())),
    elimination = purrr::map_lgl(metadata$id, ~ isTRUE(metadata$dimension[[.x]]$extension$elimination)),
    role = purrr::map_chr(metadata$id, ~ {
      if (!is.null(metadata$role$time) && .x %in% metadata$role$time) return("time")
      if (!is.null(metadata$role$metric) && .x %in% metadata$role$metric) return("contents")
      if (!is.null(metadata$role$geo) && .x %in% metadata$role$geo) return("geo")
      "other"
    }),
    show = purrr::map_chr(metadata$dimension, ~ .x$extension$show %||% NA_character_)
  )
  
  variables <- variables |> 
    dplyr::mutate(role = dplyr::if_else(tolower(code) %in% c("region"), "geo", role))
  
  return(variables)
}

#' Get the valid values of a PxWeb table's variables
#'
#' @param table Table id or a metadata object from `pxweb2_get_metadata()`. Obligatorisk.
#' @param variables Optional vector of variable names if you do not want values
#'   for all variables.
#' @param return_df_if_only_one_variable If `TRUE`, a `tibble` is returned
#'   instead of a list when only one variable is requested.
#' @param include_aggregations Controls fetching of aggregations: `"auto"`,
#'   `"all"`, `"none"`, `"codelists"`, or a named vector per variable.
#' @param auto_limit Max number of code-list calls for `"auto"`.
#' @param aggregation_member_sep Character(s) that separate members in an
#'   aggregation.
#' @param base_url Provider id or base URL, see `pxweb2_base_url()`.
#' @param lang Language code, used when `table` is a table id.
#' @param quiet If `TRUE`, suppresses the `include_aggregations = "auto"`
#'   summary message and the "aggregation code lists not fetched in full"
#'   notice. Default `FALSE` keeps today's behaviour. Does not suppress the
#'   "variables ... do not exist in the table" notice, since that one usually
#'   points at a real typo in `variables`.
#'
#' @return A named list (or `tibble`, see `return_df_if_only_one_variable`) of
#'   valid values per variable.
#' @noRd
pxweb2_get_values <- function(
    table,                               # a table id or a metadata object from pxweb2_get_metadata()
    variables = NULL,                    # variable names if you do not want values for all variables
    return_df_if_only_one_variable = TRUE,
    include_aggregations = "auto",       # "auto"      = fetch everything if the total number of code lists <= auto_limit, otherwise "codelists"
    # "all"       = always fetch all aggregations in full
    # "none"      = nothing, not even code-list metadata
    # "codelists" = only code-list metadata (id + label, type "AggregationCodelist"), no members
    # named vector, e.g. c(Region = "agg_RegionLA2018", Alder = "all", Tid = "codelists")
    #   => selective control per variable; the name can be a code or a label (case-insensitive)
    #   => value per variable: "all", "codelists", or one/several agg ids (e.g. "agg_RegionLA2018")
    auto_limit = 30L,                    # max number of code-list calls for "auto"
    quiet = FALSE,                       # TRUE suppresses the include_aggregations = "auto" messages below
    aggregation_member_sep = ";",        # aggregation members are separated with this character(s)
    base_url = .pxweb2_default_base_url(),
    lang = NULL                          # language code, used when table is a table id
) {

  if (is.null(table)) stop("table must be supplied, either as a table id or a metadata object.")
  if (!is.list(table)) {
    metadata <- pxweb2_get_metadata(table, base_url = base_url, lang = lang)
  } else metadata <- table

  # all variables: code + label
  variables_df <- tibble::tibble(
    code  = names(metadata$dimension),
    label = purrr::map_chr(metadata$dimension, ~ .x$label %||% NA_character_)
  )

  table_variables <- metadata$dimension

  # filter variables if the variables parameter is set
  if (!is.null(variables)) {
    var_not_found <- variables[!tolower(variables) %in% tolower(variables_df$code) &
                                !tolower(variables) %in% tolower(variables_df$label)]
    var_found <- variables[tolower(variables) %in% tolower(variables_df$code) |
                             tolower(variables) %in% tolower(variables_df$label)]
    if (length(var_not_found) > 0) {
      cat(paste0("The variables ", paste0(var_not_found, collapse = ", "), " do not exist in the table and are therefore omitted."))
    }
    if (length(var_found) > 0) {
      table_variables <- table_variables[
        tolower(names(table_variables)) %in% tolower(var_found) |
          tolower(purrr::map_chr(table_variables, "label")) %in% tolower(var_found)
      ]
    }
  }

  # ---------------------------------------------------------------------------
  # helper: normalise variable name/code -> code
  .to_var_code <- function(name) {
    hits <- variables_df$code[
      tolower(variables_df$code)  %in% tolower(name) |
        tolower(variables_df$label) %in% tolower(name)
    ]
    hits
  }

  # helper: extract code-list info (id + label + url) from a dimension element
  .cl_info <- function(dim_el) {
    cl <- purrr::pluck(dim_el, "extension", "codelists", .default = NULL)
    if (is.null(cl) || length(cl) == 0) {
      return(tibble::tibble(code = character(), label = character(),
                            type = character(), url = character()))
    }
    purrr::map_df(cl, function(item) {
      lbl <- purrr::pluck(item, "label", .default = NA)
      if (is.list(lbl)) {
        lbl <- purrr::pluck(lbl, lang %||% "sv",
                            .default = (unlist(lbl, use.names = FALSE)[1] %||% NA_character_))
      }
      tibble::tibble(
        code  = purrr::pluck(item, "id",    .default = NA_character_),
        label = lbl %||% NA_character_,
        type  = purrr::pluck(item, "type",  .default = NA_character_),
        url   = purrr::pluck(item, "links", 1, "href", .default = NA_character_)
      )
    }) |>
      dplyr::filter(tolower(type) == "aggregation")
  }
  
  # ---------------------------------------------------------------------------
  # count the total number of code lists in the table (for "auto" mode)
  total_codelists <- sum(purrr::map_int(table_variables, function(dim_el) {
    nrow(.cl_info(dim_el))
  }))

  # ---------------------------------------------------------------------------
  # interpret include_aggregations -> per-variable instruction
  # result: a named list  var_code -> mode
  #   mode is one of: "all", "codelists", "none", or a character vector of agg ids
  #
  ia <- include_aggregations

  # backwards compatibility: TRUE -> "all", FALSE -> "none"
  if (isTRUE(ia))        ia <- "all"
  if (identical(ia, FALSE)) ia <- "none"

  # validate scalar string values
  if (is.character(ia) && is.null(names(ia)) && length(ia) == 1) {
    if (!ia %in% c("auto", "all", "none", "codelists")) {
      stop('include_aggregations: invalid value "', ia,
           '". Allowed: "auto", "all", "none", "codelists", or a named vector.', call. = FALSE)
    }
  }

  # resolve "auto": choose "all" or "codelists" depending on the total number of code lists
  auto_chose_all <- FALSE
  if (identical(ia, "auto")) {
    if (total_codelists <= auto_limit) {
      ia <- "all"
      auto_chose_all <- TRUE
    } else {
      ia <- "codelists"
    }
  }

  # build the per-variable instruction vector
  # var_mode: named character vector  var_code -> "all" | "codelists" | "none" | "agg_..."
  if (is.character(ia) && is.null(names(ia))) {
    # scalar mode ("all", "none", "codelists") -> applies to all variables
    scalar_mode <- ia
    var_mode <- stats::setNames(rep(scalar_mode, length(table_variables)),
                                names(table_variables))
  } else if (is.character(ia) && !is.null(names(ia))) {
    # named vector: c(Region = "agg_RegionLA2018", Alder = "all", ...)
    # unknown variable names -> warning; variables not mentioned -> "codelists"
    var_mode <- stats::setNames(rep("codelists", length(table_variables)),
                                names(table_variables))

    ia_names <- names(ia)
    unknown  <- ia_names[
      !tolower(ia_names) %in% tolower(variables_df$code) &
        !tolower(ia_names) %in% tolower(variables_df$label)
    ]
    if (length(unknown) > 0) {
      warning("The following variable names in include_aggregations are not recognised and are ignored: ",
              paste(unknown, collapse = ", "), call. = FALSE)
    }

    # group by variable code (there can be several entries with the same name, e.g. Region = "agg_X", Region = "agg_Y")
    for (i in seq_along(ia)) {
      var_codes <- .to_var_code(ia_names[i])
      if (length(var_codes) == 0) next
      val <- ia[[i]]

      for (vc in var_codes) {
        existing <- var_mode[[vc]]
        if (val %in% c("all", "none", "codelists")) {
          # explicit mode: always overwrite
          var_mode[[vc]] <- val
        } else {
          # specific agg id: accumulate (there can be several entries)
          if (existing %in% c("codelists", "none")) {
            var_mode[[vc]] <- val          # first agg id for this variable
          } else if (!existing %in% c("all")) {
            var_mode[[vc]] <- paste(c(existing, val), collapse = "\n")  # append
          }
          # if existing == "all" -> leave it as "all"
        }
      }
    }
  } else {
    stop('include_aggregations must be "auto", "all", "none", "codelists" or a named vector.', call. = FALSE)
  }

  # ---------------------------------------------------------------------------
  # GET -> JSON with cache
  .codelist_cache <- new.env(parent = emptyenv())
  
  fetch_codelist <- purrr::possibly(function(u) {
    if (exists(u, envir = .codelist_cache, inherits = FALSE)) {
      return(get(u, envir = .codelist_cache, inherits = FALSE))
    }
    resp <- .pxweb2_GET(u, httr::accept_json())
    httr::stop_for_status(resp)
    out <- jsonlite::fromJSON(
      httr::content(resp, "text", encoding = "UTF-8"),
      simplifyVector = FALSE
    )
    assign(u, out, envir = .codelist_cache)
    out
  }, otherwise = NULL)
  
  # helper: fetch the full contents of a selection of code lists (filtered on agg ids if given)
  .fetch_agg_values <- function(cl_info_df, agg_ids_filter = NULL) {
    # agg_ids_filter: character vector of specific agg ids to fetch, NULL = all
    df <- cl_info_df
    if (!is.null(agg_ids_filter)) {
      df <- df |> dplyr::filter(tolower(code) %in% tolower(agg_ids_filter))
    }
    if (nrow(df) == 0) {
      return(tibble::tibble(code = character(), label = character(), type = character()))
    }
    
    aggr_parsed <- df |>
      dplyr::transmute(agg_id = code, agg_label = label, url) |>
      dplyr::mutate(
        codelist = purrr::map(url, fetch_codelist),
        values_long_df = purrr::map(codelist, ~ {
          v <- .x$values
          if (is.null(v) || length(v) == 0) {
            return(tibble::tibble(code = character(), label = character(), members = character()))
          }
          purrr::map_df(v, ~ tibble::tibble(
            code    = as.character(purrr::pluck(.x, "code",  .default = NA_character_)),
            label   = as.character(purrr::pluck(.x, "label", .default = NA_character_)),
            members = {
              vm <- purrr::pluck(.x, "valueMap", .default = NULL)
              if (is.null(vm)) NA_character_
              else paste(unlist(vm, use.names = FALSE), collapse = aggregation_member_sep)
            }
          )) |>
            dplyr::filter(!is.na(code))
        })
      ) |>
      dplyr::select(-codelist)
    
    if (nrow(aggr_parsed) == 0) {
      return(tibble::tibble(code = character(), label = character(), type = character()))
    }
    
    aggr_parsed |>
      dplyr::select(agg_id, agg_label, values_long_df) |>
      tidyr::unnest(values_long_df) |>
      dplyr::mutate(type = "Aggregation") |>
      dplyr::relocate(c(code, label, type), .before = 1)
  }
  
  # ---------------------------------------------------------------------------
  # build the list: one tibble per dimension element
  vars_with_codelist_only <- character(0)

  values <- purrr::imap(table_variables, function(dim_el, dim_name) {

    # --- variable values (type = "Variable")
    cat_lab <- purrr::pluck(dim_el, "category", "label", .default = NULL)
    cat_df <- if (is.null(cat_lab)) {
      tibble::tibble(code = character(), label = character(), type = character())
    } else {
      v <- unlist(cat_lab, use.names = TRUE)
      tibble::tibble(code = names(v), label = unname(v), type = "Variable")
    }

    mode <- var_mode[[dim_name]] %||% "codelists"
    cl_info <- .cl_info(dim_el)

    if (mode == "none") {
      return(cat_df)
    }

    if (mode == "codelists") {
      if (nrow(cl_info) == 0) return(cat_df)
      vars_with_codelist_only <<- c(vars_with_codelist_only, dim_name)
      codelist_meta <- cl_info |>
        dplyr::select(code, label, type) |>
        dplyr::mutate(type = "AggregationCodelist")
      return(dplyr::bind_rows(cat_df, codelist_meta))
    }

    if (mode == "all") {
      if (nrow(cl_info) == 0) return(cat_df)
      return(dplyr::bind_rows(cat_df, .fetch_agg_values(cl_info)))
    }

    # specific agg ids (one or several, newline-separated internally)
    agg_ids <- unlist(stringr::str_split(mode, "\n"), use.names = FALSE)
    unknown_ids <- agg_ids[!tolower(agg_ids) %in% tolower(cl_info$code)]
    if (length(unknown_ids) > 0) {
      warning("The following agg ids are not recognised for the variable '", dim_name, "' and are ignored: ",
              paste(unknown_ids, collapse = ", "), call. = FALSE)
    }
    agg_ids <- agg_ids[tolower(agg_ids) %in% tolower(cl_info$code)]

    if (length(agg_ids) == 0) {
      # no valid agg ids -> fall back on code-list metadata
      if (nrow(cl_info) > 0) {
        vars_with_codelist_only <<- c(vars_with_codelist_only, dim_name)
        codelist_meta <- cl_info |>
          dplyr::select(code, label, type) |>
          dplyr::mutate(type = "AggregationCodelist")
        return(dplyr::bind_rows(cat_df, codelist_meta))
      }
      return(cat_df)
    }

    dplyr::bind_rows(cat_df, .fetch_agg_values(cl_info, agg_ids_filter = agg_ids))
  })

  # ---------------------------------------------------------------------------
  # messages
  if (!quiet && auto_chose_all) {
    message(
      "include_aggregations = \"auto\": found ", total_codelists,
      " code lists (<= ", auto_limit, ") and fetched all aggregations automatically."
    )
  } else if (!quiet && length(vars_with_codelist_only) > 0) {
    message(
      "The following variables have aggregation code lists that were not fetched in full: ",
      paste(vars_with_codelist_only, collapse = ", "), ".\n",
      "Set include_aggregations = \"all\" (or name the variables with specific agg ids) ",
      "to fetch all aggregations and their values."
    )
  }

  if (return_df_if_only_one_variable && length(values) == 1) {
    values <- tibble::as_tibble(values[[1]])
  }

  return(values)
}




#' Build a query-list template as text for a script
#'
#' Builds the text representation of a `query` list from a table's variables, to
#' paste into a script. Printed with `cat()`.
#'
#' @param table_id Table id. Obligatorisk.
#' @param default_value Value used for variables without an override, normally
#'   `"*"`.
#' @param overrides Named list of values per variable.
#' @param object_name Name of the list object in the generated text.
#' @param base_url Provider id or base URL, see `pxweb2_base_url()`.
#' @param lang Language code; `NULL` = the provider's default language.
#'
#' @return Called for its side effect (`cat()`). Returns `NULL` invisibly.
#' @noRd
pxweb2_query_list_template <- function(table_id,
                                         default_value = "*",
                                         overrides = list(),
                                         object_name = "query_list",
                                         base_url = .pxweb2_default_base_url(),
                                         lang = NULL
) {
  # create the text for a query list for a script, based on the given table
  variables_df <- pxweb2_get_variables(table = table_id, base_url = base_url, lang = lang)
  vars <- variables_df$code
  
  vals <- purrr::map_chr(vars, function(var) {
    v <- if (!is.null(overrides[[var]])) overrides[[var]] else default_value
    
    if (length(v) == 1 && is.na(v)) return(NA_character_)
    
    if (is.character(v) && length(v) == 1) {
      paste0(var, ' = "', v, '"')
    } else {
      paste0(
        var, " = c(",
        paste(sprintf('"%s"', as.character(v)), collapse = ", "),
        ")"
      )
    }
  })
  
  vals <- vals[!is.na(vals)]
  
  cat(
    paste0(
      object_name, " <- list(\n  ",
      paste(vals, collapse = ",\n  "),
      "\n)"
    )
  )
}

#' Build a ready-made pxr_pxweb2_get_data() call as text
#'
#' Generates a complete `pxr_pxweb2_get_data()` call for a table, as text. Printed
#' with `cat()` and, by default, copied to the clipboard.
#'
#' @param table_id Table id. Obligatorisk.
#' @param default_value Value used for variables without an override, normally
#'   `"*"`.
#' @param overrides Named list of values per variable.
#' @param to_clipboard If `TRUE`, the text is copied to the clipboard. Requires
#'   the `clipr` package and an available clipboard (on Linux `xclip`, `xsel` or
#'   `wl-clipboard` plus an active display). If not available, the text is only
#'   printed.
#' @param base_url Provider id or base URL, see `pxweb2_base_url()`. Written
#'   into the generated call together with `lang`.
#' @param lang Language code; `NULL` = the provider's default language.
#'
#' @return Called for its side effect (`cat()`). Returns `NULL` invisibly.
#' @noRd
pxweb2_data_script_template <- function(table_id,
                                          default_value = "*",
                                          overrides = list(),
                                          to_clipboard = TRUE,
                                          base_url = .pxweb2_default_base_url(),
                                          lang = NULL
) {

  base_url <- .pxweb2_normalise_base_url(base_url)
  lang <- .pxweb2_resolve_lang(lang, base_url)
  meta <- pxweb2_get_metadata(table_id, base_url = base_url, lang = lang)
  title_txt <- meta$label
  variables_df <- pxweb2_get_variables(meta)
  # Ingen anledning att hämta aggregeringar eller skriva ut include_aggregations-meddelandet här -
  # funktionen genererar bara ett skript utifrån variablernas vanliga värden.
  values_df <- pxweb2_get_values(meta, include_aggregations = "none", quiet = TRUE)
  vars <- variables_df$code
  
  vals <- purrr::map_chr(vars, function(var) {
    v <- if (!is.null(overrides[[var]])) overrides[[var]] else default_value
    
    if (length(v) == 1 && is.na(v)) return(NA_character_)
    
    if (is.character(v) && length(v) == 1) {
      paste0(var, ' = "', v, '"')
    } else {
      paste0(
        var, " = c(",
        paste(sprintf('"%s"', as.character(v)), collapse = ", "),
        ")"
      )
    }
  })
  
  vals <- vals[!is.na(vals)]
  
  return_txt <- paste0(
    "# ", title_txt, "\n",
    "dataset_df <- pxr_pxweb2_get_data(\n",
    '\ttable = "', table_id, '",\n',
    "\tquery = list(\n\t\t",
    paste(vals, collapse = ",\n\t\t"),
    "\n\t\t),\n",
    '\tlang = "', lang, '",\n',
    '\tbase_url = "', base_url, '")'
  )

  if (to_clipboard) {
    if (requireNamespace("clipr", quietly = TRUE) && clipr::clipr_available()) {
      clipr::write_clip(return_txt)
    } else {
      message("Clipboard not available - the text is only printed.")
    }
  }

  cat(return_txt)
}


#' Search among tables in a PxWeb API
#'
#' Queries the tables endpoint and pages through all matches. API v1 has no
#' such endpoint, so there the whole list is built first (see
#' `.pxweb1_all_tables()`) and `query` and `pastDays` are applied to it.
#'
#' @param query Free-text search. `NULL` lists all tables.
#' @param base_url Provider id or base URL, see `pxweb2_base_url()`.
#' @param lang Language code; `NULL` = the provider's default language.
#' @param pastDays Restrict to tables updated in the last N days.
#' @param includeDiscontinued Whether discontinued tables should be included.
#' @param pageSize Number of matches per page.
#' @param timeout_sec Timeout per call in seconds.
#' @param max_pages Max number of pages to fetch.
#'
#' @return A `tibble` with one row per table.
#' @noRd
pxweb2_search_tables <- function(query = NULL,
                                 base_url = .pxweb2_default_base_url(),
                                 lang = NULL,
                                 pastDays = NULL,
                                 includeDiscontinued = NULL,
                                 pageSize = 200,
                                 timeout_sec = 60,
                                 max_pages = Inf) {
  
  base_url <- .pxweb2_normalise_base_url(base_url)
  lang <- .pxweb2_resolve_lang(lang, base_url)
  if (.dst_is_url(base_url)) {
    return(.dst_search_tables(query, lang, pastDays, includeDiscontinued, timeout_sec))
  }
  if (.pxweb1_is_url(base_url)) {
    return(.pxweb1_search_tables(query, base_url, lang, pastDays, timeout_sec))
  }
  base_url <- sub("/+$", "", base_url)

  qs_base <- purrr::compact(list(
    lang = lang,
    query = query,
    pastDays = pastDays,
    includeDiscontinued = includeDiscontinued,
    pageSize = pageSize
  ))
  
  fetch_page <- function(pageNumber) {
    resp <- .pxweb2_GET(
      url = base_url,
      query = c(qs_base, list(pageNumber = pageNumber)),
      httr::accept_json(),
      httr::timeout(timeout_sec)
    )
    httr::stop_for_status(resp)
    
    txt <- httr::content(resp, "text", encoding = "UTF-8")
    jsonlite::fromJSON(txt, simplifyVector = TRUE)
  }
  
  out1 <- fetch_page(1)
  
  # pick out tables whether the response is in $tables or "directly"
  extract_tables <- function(out) {
    if (is.data.frame(out)) return(tibble::as_tibble(out))
    if (is.list(out) && "tables" %in% names(out)) {
      if (is.data.frame(out$tables)) return(tibble::as_tibble(out$tables))
      if (is.list(out$tables)) return(tibble::as_tibble(out$tables))
    }
    if (is.list(out)) return(tibble::as_tibble(out))
    tibble::tibble()
  }
  
  page1 <- extract_tables(out1)
  
  # decide whether there are more pages: the API's page$totalPages if given
  # (the API may also cap pageSize), otherwise stop when a page is not full
  if (nrow(page1) == 0) return(page1)
  total_pages <- suppressWarnings(as.numeric(
    if (is.list(out1) && !is.data.frame(out1)) out1$page$totalPages else NULL
  ))
  if (length(total_pages) != 1 || is.na(total_pages)) total_pages <- NA_real_

  pages <- list(page1)
  page <- 2
  
  while (page <= max_pages) {
    if (!is.na(total_pages) && page > total_pages) break
    if (is.na(total_pages) && nrow(pages[[length(pages)]]) < pageSize) break
    
    outn <- fetch_page(page)
    pgn <- extract_tables(outn)
    
    if (nrow(pgn) == 0) break
    pages[[length(pages) + 1]] <- pgn
    
    if (is.na(total_pages) && nrow(pgn) < pageSize) break
    page <- page + 1
  }
  
  dplyr::bind_rows(pages) |> dplyr::distinct()
}

# function helpers to create queries for aggregations as well
# turns a request into a POST: every codelist[var] in extra_query is moved into
# the body as the "codelist" field of that variable's selection. Anything else
# in extra_query (outputValues[var]) stays as query parameters.
.pxweb2_post_request <- function(req) {
  body <- req$body
  extra <- req$extra_query %||% list()
  is_codelist <- grepl("^codelist\\[", names(extra))

  for (key in names(extra)[is_codelist]) {
    var <- sub("^codelist\\[(.*)\\]$", "\\1", key)
    for (i in seq_along(body$selection)) {
      if (identical(body$selection[[i]]$variableCode, var)) {
        body$selection[[i]]$codelist <- as.character(extra[[key]])
      }
    }
  }

  list(body = body, query = extra[!is_codelist])
}

.pxweb2_request <- function(body, extra_query = list(), tag = list()) {
  list(body = body, extra_query = extra_query, tag = tag)
}

.pxweb2_get_valuecodes <- function(query_list, var) {
  i <- which(purrr::map_chr(query_list$selection, "variableCode") == var)
  if (length(i) == 0) return(NULL)
  unlist(query_list$selection[[i]]$valueCodes, use.names = FALSE)
}

.pxweb2_set_valuecodes <- function(query_list, var, vals) {
  i <- which(purrr::map_chr(query_list$selection, "variableCode") == var)
  if (length(i) == 0) return(query_list)
  
  query_list$selection[[i]]$valueCodes <- if (length(vals) == 1 && is.na(vals)) {
    NULL
  } else if (length(vals) == 0) {
    list()
  } else if (length(vals) == 1 && identical(vals, "*")) {
    list("*")
  } else if (is.character(vals) && length(vals) == 1) {
    list(vals)
  } else {
    as.list(as.character(vals))
  }
  
  query_list
}


.pxweb2_make_request_chunks <- function(variables_df, request_list, valid_values_list, max_cells = 150000) {

  request_list <- purrr::compact(request_list)

  # Plain lapply() rather than purrr::map(): .pxweb2_make_chunks() raises a
  # deliberate, already-informative error when a request cannot be brought
  # under max_cells (see "Too many cells selected" there). purrr::map() would
  # wrap that in its own "Error in `purrr::map()` / In index: n" framing,
  # which only adds noise around a controlled stop() - it is not a mapping
  # bug to be located by index.
  request_list |>
    lapply(function(.req) {
      bodies <- .pxweb2_make_chunks(
        variables_df,
        .req$body,
        valid_values_list = valid_values_list,
        max_cells = max_cells
      )
      bodies <- purrr::compact(bodies)

      purrr::map(bodies, ~ .pxweb2_request(
        body = .x,
        extra_query = .req$extra_query,
        tag = .req$tag
      ))
    }) |>
    purrr::flatten()
}

.pxweb2_is_special_value <- function(vals) {
  length(vals) == 1 && (
    identical(vals, "*") ||
      identical(vals, "**") ||
      grepl("^\\s*(top|bottom)\\s*\\(\\s*\\d+\\s*\\)\\s*$", vals, ignore.case = TRUE) ||
      grepl("^agg_", vals)
  )
}

.pxweb2_var_alternatives <- function(query_list,
                                           valid_values_list,
                                           var,
                                           output_values = "aggregated",    # can also be single
                                           warn = TRUE) {
  
  vals <- .pxweb2_get_valuecodes(query_list, var)
  if (is.null(vals) || length(vals) == 0) {
    return(list(list(var = var, vals = NULL, extra_query = list())))
  }
  
  vals <- as.character(vals)
  
  # special: top/bottom/* -> do nothing (no codelist)
  if (length(vals) == 1 && (identical(vals, "*") ||
                            grepl("^\\s*(top|bottom)\\s*\\(\\s*\\d+\\s*\\)\\s*$", vals, ignore.case = TRUE))) {
    return(list(list(var = var, vals = vals, extra_query = list())))
  }
  
  ok_tbl <- valid_values_list[[var]]
  if (is.null(ok_tbl) || !all(c("code", "type") %in% names(ok_tbl))) {
    return(list(list(var = var, vals = vals, extra_query = list())))
  }
  
  agg_tbl <- ok_tbl |>
    dplyr::filter(tolower(type) == "aggregation") |>
    dplyr::select(dplyr::any_of(c("code", "agg_id","agg_label")))
  
  has_agg <- all(c("code", "agg_id") %in% names(agg_tbl)) && nrow(agg_tbl) > 0
  
  agg_code_to_id <- if (has_agg) stats::setNames(agg_tbl$agg_id, agg_tbl$code) else character()
  
  # 1) var="**" => default "*" + one per agg_id with "*"
  if (length(vals) == 1 && identical(vals, "**")) {
    if (!has_agg) {
      if (warn) warning("The variable `", var, "` has no aggregations; '**' is interpreted as '*'.")
      vals <- "*"
    } else {
      all_agg_ids <- unique(agg_tbl$agg_id)
      
      out <- list(
        list(var = var, vals = "*", extra_query = list())  # default
      )
      
      out <- c(out, purrr::map(all_agg_ids, ~ list(
        var = var,
        vals = "*",
        extra_query = stats::setNames(list(.x), paste0("codelist[", var, "]")) |>
          c(stats::setNames(list(output_values), paste0("outputValues[", var, "]")))
      )))
      
      return(out)
    }
  }
  
  # 2) var="agg_..." => one alt: vals="*" + codelist[var]=agg_...
  if (length(vals) == 1 && grepl("^agg_", vals)) {
    if (!has_agg) {
      if (warn) warning("The variable `", var, "` has no aggregations; `", vals, "` is ignored.")
      return(list(list(var = var, vals = "*", extra_query = list())))
    }
    return(list(list(
      var = var,
      vals = "*",
      extra_query = stats::setNames(list(vals), paste0("codelist[", var, "]")) |>
        c(stats::setNames(list(output_values), paste0("outputValues[", var, "]")))
    )))
  }
  
  
  # 3) mixed codes: split into default + per agg_id (for agg codes)
  is_agg <- vals %in% names(agg_code_to_id)
  default_vals <- vals[!is_agg]
  agg_vals <- vals[is_agg]
  
  out <- list()
  
  if (length(default_vals) > 0) {
    out <- c(out, list(list(var = var, vals = default_vals, extra_query = list())))
  }
  
  if (length(agg_vals) > 0) {
    by_id <- split(agg_vals, agg_code_to_id[agg_vals])
    out <- c(out, purrr::imap(by_id, ~ list(
      var = var,
      vals = .x,
      extra_query = stats::setNames(list(.y), paste0("codelist[", var, "]")) |>
        c(stats::setNames(list(output_values), paste0("outputValues[", var, "]")))
    )))
  }
  
  # if no agg matched -> default only
  if (length(out) == 0) out <- list(list(var = var, vals = vals, extra_query = list()))

  # if several agg_id in the same variable (codes from different aggs) => return several alternatives
  out
}


.pxweb2_expand_requests_generic <- function(query_list, valid_values_list,
                                                  output_values = "aggregated",    # can also be single
                                                  warn = TRUE) {

  vars_in_body <- purrr::map_chr(query_list$selection, "variableCode")

  # build alternatives per variable
  alts <- purrr::map(vars_in_body, ~ .pxweb2_var_alternatives(
    query_list, valid_values_list, var = .x,
    output_values = output_values, warn = warn
  ))
  names(alts) <- vars_in_body

  # cartesian product of alternatives (usually this is 1)
  combos <- tidyr::expand_grid(!!!alts) |> purrr::transpose()
  
  purrr::map(combos, function(choice) {
    body  <- query_list
    extra <- list()
    
    for (opt in choice) {
      if (!is.null(opt$vals)) {
        body <- .pxweb2_set_valuecodes(body, opt$var, opt$vals)
      }
      if (length(opt$extra_query) > 0) {
        extra <- c(extra, opt$extra_query)
      }
    }
    
    .pxweb2_request(body = body, extra_query = extra, tag = list(kind = "expanded"))
  })
}

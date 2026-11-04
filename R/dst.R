# Statistics Denmark (Danmarks Statistik), https://api.statbank.dk/v1/.
#
# Not PxWeb, but close enough that the rest of the package can work on the same
# metadata and table lists. The differences are hidden here:
#   - tableinfo/<table> gives the metadata, .pxweb1_metadata() converts it
#   - tables gives the table list, with update dates, periods and an active flag;
#     subjects gives the folders the tables are in
#   - data is fetched with POST to data, body {table, format, lang, variables},
#     and comes as JSON-stat 1 (see .pxweb2_parse_jsonstat2())
#   - the limit is 1 000 000 cells counted as rows x columns, see .dst_max_rows()
#   - no code lists, so no aggregations

.dst_base_url <- "https://api.statbank.dk/v1/"

.dst_is_url <- function(url) {
  is.character(url) && length(url) == 1 && !is.na(url) &&
    grepl("^https?://api\\.statbank\\.dk(/|$)", trimws(url))
}

# There is no endpoint for the limits, and no stated limit on calls, so the
# package's usual pace of calls is kept.
.dst_read_config <- function() {
  list(maxDataCells = 1000000, maxCallsPerTimeWindow = 30L, timeWindow = 10,
       languages = list(list(id = "da", label = "Dansk"), list(id = "en", label = "English")),
       defaultLanguage = "da")
}

# The API counts rows x columns, with a column per variable and one for the
# value. One more is added as a margin, since it counts before elimination.
.dst_max_rows <- function(variables_df, limit = 1000000) {
  floor(limit / (nrow(variables_df) + 2))
}

.dst_get <- function(path, query, timeout_sec = 60) {
  .pxweb2_GET(paste0(.dst_base_url, path), httr::accept_json(), httr::timeout(timeout_sec),
              query = c(list(format = "JSON"), query))
}

.dst_get_metadata <- function(table_id, lang, use_cache = TRUE) {
  key <- paste0(.dst_base_url, "tableinfo/", table_id, "?lang=", lang)

  if (isTRUE(use_cache)) {
    cached <- .pxweb2_cache_get(key)
    if (!is.null(cached)) return(cached)
  }

  resp <- .dst_get(paste0("tableinfo/", utils::URLencode(table_id, reserved = TRUE)), list(lang = lang))
  if (httr::status_code(resp) %in% c(400L, 404L)) {
    stop("Table id '", table_id, "' was not found at Statistics Denmark (HTTP ",
         httr::status_code(resp), "). Find the id with pxr_search_tabs().", call. = FALSE)
  }
  httr::stop_for_status(resp)

  info <- .pxweb1_parse(resp)
  raw <- list(
    title = info$text,
    variables = lapply(info$variables, function(v) {
      list(code = v$id, text = v$text, elimination = v$elimination, time = v$time, map = v$map,
           values = lapply(v$values, `[[`, "id"), valueTexts = lapply(v$values, `[[`, "text"))
    })
  )
  meta <- .pxweb1_metadata(raw, info$id %||% table_id)
  meta$updated <- if (!is.null(info$updated)) paste0(info$updated, "Z")
  # the same form as a note in API v2, for pxr_get_comments()
  meta$note <- info$footnote$text
  if (isTRUE(info$footnote$mandatory)) meta$extension$noteMandatory <- list(`0` = TRUE)
  .pxweb2_cache_set(key, meta)
}

# Folders per table from the subject tree: table id -> list of data frames
# (id, label), as `paths` in the v2 table list. A table can be in several folders.
.dst_table_paths <- function(lang, timeout_sec = 60) {
  resp <- tryCatch(
    .dst_get("subjects", list(lang = lang, recursive = "true", includeTables = "true"), timeout_sec),
    error = function(e) NULL
  )
  if (is.null(resp) || httr::status_code(resp) != 200) return(list())

  paths <- list()
  visit <- function(node, ids, labels) {
    ids <- c(ids, as.character(node$id))
    labels <- c(labels, trimws(as.character(node$description)))
    for (tab in node$tables) {
      paths[[tab$id]] <<- c(paths[[tab$id]],
                            list(data.frame(id = ids, label = labels, stringsAsFactors = FALSE)))
    }
    for (child in node$subjects) visit(child, ids, labels)
  }
  for (node in .pxweb1_parse(resp)) visit(node, character(), character())
  paths
}

# All tables, active and inactive, kept in the session cache
.dst_all_tables <- function(lang, timeout_sec = 60) {
  key <- paste0(.dst_base_url, "tables?lang=", lang)
  cached <- .pxweb2_cache_get(key)
  if (!is.null(cached)) return(cached)

  resp <- .dst_get("tables", list(lang = lang, includeInactive = "true"), timeout_sec)
  httr::stop_for_status(resp)
  rows <- .pxweb1_parse(resp)
  paths <- .dst_table_paths(lang, timeout_sec)

  chr <- function(name) vapply(rows, function(r) as.character(r[[name]] %||% NA_character_)[1], "")
  ids <- chr("id")
  tables <- tibble::tibble(
    id = ids, label = chr("text"), updated = chr("updated"),
    firstPeriod = chr("firstPeriod"), lastPeriod = chr("latestPeriod"),
    discontinued = !vapply(rows, function(r) isTRUE(r$active), TRUE),
    subjectCode = vapply(ids, function(i) as.character(paths[[i]][[1]]$id[1] %||% NA_character_), "",
                         USE.NAMES = FALSE),
    paths = unname(lapply(ids, function(i) paths[[i]] %||% list()))
  )
  .pxweb2_cache_set(key, tables)
}

# Counterpart of the /tables endpoint. Inactive tables are left out unless asked
# for, as discontinued ones are in API v2.
.dst_search_tables <- function(query, lang, pastDays = NULL, includeDiscontinued = NULL, timeout_sec = 60) {
  tables <- .dst_all_tables(lang, timeout_sec)
  if (!isTRUE(as.logical(includeDiscontinued))) tables <- tables[!tables$discontinued, , drop = FALSE]
  .pxweb1_filter_tables(tables, query, pastDays)
}

# The request body of a query_list in v2 form. In this API "(2)" means the second
# latest period, not the two latest, so top(n) and bottom(n) become lists of codes.
.dst_post_body <- function(body, table, lang, valid_values_list) {
  variables <- lapply(body$selection, function(s) {
    vals <- as.character(unlist(s$valueCodes, use.names = FALSE))
    m <- if (length(vals) == 1) {
      regmatches(vals, regexec("^\\s*(top|bottom)\\s*\\(\\s*(\\d+)\\s*\\)\\s*$", vals, ignore.case = TRUE))[[1]]
    }
    if (length(m) == 3) {
      codes <- valid_values_list[[s$variableCode]]$code
      pick <- if (tolower(m[2]) == "top") utils::tail else utils::head
      vals <- pick(codes, as.integer(m[3]))
    }
    list(code = s$variableCode, values = as.list(vals))
  })
  list(table = table, format = "JSONSTAT", lang = lang, variables = variables)
}

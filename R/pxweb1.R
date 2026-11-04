# PxWeb API v1 (also the protocol behind SSB's /api/v0/).
#
# Used only for providers without API v2, see .pxweb1_latest_url().
#
# v1 differs from v2 in these ways, and the functions here hide the differences
# so that the rest of the package works on the same metadata and table lists:
#   - the language and database are part of the URL: <prefix>/<lang>/<db>/<path>
#   - a table id is the table's path below base_url, e.g.
#     "indikatorer/Indikatorm.px"; see .pxweb1_resolve_id() for ids without folders
#   - there is no list of tables: folders are read one at a time (or, on sites
#     that allow it, through the search ?query=*&filter=*)
#   - metadata is {title, variables}; .pxweb1_metadata() turns it into the same
#     shape as the json-stat2 metadata of v2
#   - data is fetched with POST to the table's URL, body {query, response}
#   - no code lists, so no aggregations; no /config languages, no discontinued
#     flag and no first or last period in the table list

# Statistics Denmark has /v1/ in its address too, but is not PxWeb, see dst.R
.pxweb1_is_url <- function(url) {
  is.character(url) && length(url) == 1 && !is.na(url) && !.dst_is_url(url) &&
    grepl("^https?://.+/v[01](/|$)", trimws(url))
}

# ".../v1/doris/sv/ssd/BE" -> prefix ".../v1/doris/", lang "sv", rest "ssd/BE".
# The language is the first two-letter folder after v1, at most one folder
# later (as "doris" here). NULL if there is none.
.pxweb1_parts <- function(url) {
  url <- sub("/+$", "", sub("[?#].*$", "", trimws(url)))
  m <- regmatches(url, regexec("^(https?://.+?/v[01]/(?:[^/]+/)??)([A-Za-z]{2})(?:/(.*))?$",
                               url, perl = TRUE))[[1]]
  if (length(m) == 0) return(NULL)
  list(prefix = m[2], lang = m[3], rest = m[4])
}

# A provider with both API v1 and v2 is always used through v2. Providers whose
# v2 has an address of its own are listed here; for others, v2 is looked for next
# to v1 (".../v1/sv/db/" -> ".../v2/config"), so that a site moving to v2 is
# followed without changing the package. The answer is kept in the session cache.
.pxweb1_v2_elsewhere <- function() {
  c("https://api.scb.se/" = "https://statistikdatabasen.scb.se/api/v2/tables/",
    "https://data.ssb.no/" = "https://data.ssb.no/api/pxwebapi/v2/tables/")
}

# A v1 URL -> the provider's v2 tables endpoint, if it has one. Anything else is
# returned as it is.
.pxweb1_latest_url <- function(url) {
  if (!.pxweb1_is_url(url)) return(url)
  root <- sub("/v[01](/.*)?$", "/v2/", sub("[?#].*$", "", trimws(url)))
  key <- paste0("latest:", root)
  latest <- .pxweb2_cache_get(key)

  if (is.null(latest)) {
    known <- .pxweb1_v2_elsewhere()
    hit <- known[startsWith(tolower(root), names(known))]
    latest <- if (length(hit) > 0) unname(hit[1]) else {
      config <- tryCatch({
        resp <- .pxweb2_GET(paste0(root, "config"), httr::accept_json(), httr::timeout(10))
        if (httr::status_code(resp) == 200) .pxweb1_parse(resp)
      }, error = function(e) NULL)
      has_v2 <- is.list(config) && !is.null(config$apiVersion %||% config$maxDataCells)
      if (has_v2) paste0(root, "tables/") else ""
    }
    .pxweb2_cache_set(key, latest)
    if (nzchar(latest)) {
      message("This provider also has PxWeb API v2, which is used instead of ", url,
              ": ", latest, ". Tables have other ids there than in API v1.")
    }
  }

  if (nzchar(latest)) latest else url
}

# The form base_url has everywhere in the package: decoded, ending in "/"
.pxweb1_normalise_url <- function(url) {
  parts <- .pxweb1_parts(url)
  if (is.null(parts)) {
    stop("The address of a PxWeb API v1 must include the language, e.g. ",
         "https://statistik.konj.se/PxWeb/api/v1/sv/KonjBar/ (got ", url, ").", call. = FALSE)
  }
  rest <- utils::URLdecode(parts$rest)
  Encoding(rest) <- "UTF-8"
  paste0(parts$prefix, parts$lang, "/", if (nzchar(rest)) paste0(rest, "/"))
}

# Address of a folder or table in the given language. Folders end in "/".
.pxweb1_url <- function(base_url, lang = NULL, path = NULL, folder = is.null(path)) {
  parts <- .pxweb1_parts(base_url)
  segments <- unlist(strsplit(c(parts$rest, path), "/", fixed = TRUE), use.names = FALSE)
  segments <- segments[nzchar(segments)]
  encoded <- vapply(segments, function(s) {
    utils::URLencode(enc2utf8(s), reserved = TRUE, repeated = TRUE)
  }, "", USE.NAMES = FALSE)
  paste0(parts$prefix, lang %||% parts$lang, "/", paste(encoded, collapse = "/"),
         if (folder && length(encoded) > 0) "/")
}

# The same site whatever language the URL has, to find it among the providers
.pxweb1_site_key <- function(url) {
  parts <- if (.pxweb1_is_url(url)) .pxweb1_parts(url)
  if (is.null(parts)) return(url)
  paste0(parts$prefix, "*/", parts$rest)
}

# Parsed JSON. PxWeb v1 sites often start their responses with a byte order mark.
.pxweb1_parse <- function(resp) {
  jsonlite::fromJSON(.pxweb1_strip_bom(httr::content(resp, "text", encoding = "UTF-8")),
                     simplifyVector = FALSE)
}

.pxweb1_strip_bom <- function(txt) sub(paste0("^", intToUtf8(0xFEFF)), "", txt)

# ?config has other names than /config in v2. The limit per call is maxCells:
# Statistics Sweden and the Swedish Board of Agriculture both answer requests
# with more cells than maxValues, and Statistics Sweden refuses more than maxCells.
.pxweb1_read_config <- function(base_url) {
  resp <- .pxweb2_GET(paste0(.pxweb1_url(base_url), "?config"), httr::accept_json())
  if (httr::status_code(resp) != 200) return(NULL)
  config <- .pxweb1_parse(resp)
  config$maxDataCells <- config$maxCells %||% config$maxValues
  config$maxCallsPerTimeWindow <- config$maxCalls
  config
}

# v1 has no list of languages. A known provider gets its languages from the
# provider list; otherwise only the language in the URL is known, and
# attr(, "complete") = FALSE tells .pxweb2_resolve_lang() not to reject others.
.pxweb1_languages <- function(base_url) {
  own <- .pxweb1_parts(base_url)$lang
  prov <- pxweb2_providers()
  hit <- match(.pxweb1_site_key(base_url), vapply(prov$tables_url, .pxweb1_site_key, ""))
  ids <- if (is.na(hit) || is.na(prov$languages[hit])) character() else {
    trimws(strsplit(prov$languages[hit], ",")[[1]])
  }
  complete <- length(ids) > 0
  ids <- unique(c(ids, own))
  out <- data.frame(id = ids, label = ids, stringsAsFactors = FALSE)
  attr(out, "default") <- own
  attr(out, "complete") <- complete
  out
}

.pxweb1_time_names <- c("tid", "\u00e5r", "ar", "kalender\u00e5r", "l\u00e4s\u00e5r", "m\u00e5nad", "kvartal",
                        "period", "time", "year", "month")

# {title, variables} -> the same shape as the json-stat2 metadata from API v2.
# v1 marks the time variable but not the contents variable, which is found by
# its usual names. A variable with a map is geographical.
.pxweb1_metadata <- function(raw, table_id) {
  variables <- raw$variables
  codes <- vapply(variables, function(v) as.character(v$code), "")
  dimension <- lapply(variables, function(v) {
    values <- as.character(unlist(v$values, use.names = FALSE))
    texts <- as.character(unlist(v$valueTexts %||% v$values, use.names = FALSE))
    list(
      label = v$text %||% v$code,
      category = list(
        index = as.list(stats::setNames(seq_along(values) - 1L, values)),
        label = as.list(stats::setNames(texts, values))
      ),
      extension = list(elimination = isTRUE(v$elimination))
    )
  })
  names(dimension) <- codes
  is_time <- vapply(variables, function(v) isTRUE(v$time), TRUE)
  # some sites mark no variable as time: then the first with a usual name is
  if (!any(is_time)) {
    labels <- vapply(variables, function(v) as.character(v$text %||% v$code), "")
    hit <- tolower(codes) %in% .pxweb1_time_names | tolower(labels) %in% .pxweb1_time_names
    if (any(hit)) is_time[which(hit)[1]] <- TRUE
  }
  is_metric <- tolower(codes) %in% c("contentscode", "tiedot")
  is_geo <- vapply(variables, function(v) !is.null(v$map), TRUE)
  list(
    version = "2.0", class = "dataset", label = raw$title %||% table_id,
    role = list(time = as.list(codes[is_time]), metric = as.list(codes[is_metric]),
                geo = as.list(codes[is_geo])),
    id = as.list(codes),
    size = as.list(unname(lengths(lapply(dimension, function(d) d$category$label)))),
    dimension = dimension,
    extension = list(px = list(tableid = table_id))
  )
}

# An id without folders -> the table's whole path, from the table list. Sites differ
# in what they do with such an id: most find a table from its name alone and take the
# first of that name, and some only with ".px". Tables in different folders can have
# the same name and still differ, so such an id is an error. An id that is not in the
# list, or a list that cannot be read, is left as it is for the site to answer.
.pxweb1_resolve_id <- function(table_id, base_url, lang) {
  if (grepl("/", gsub("^/+|/+$", "", table_id))) return(table_id)
  tables <- tryCatch(.pxweb1_all_tables(base_url, lang)$id, error = function(e) character())
  if (table_id %in% tables) return(table_id)
  name <- function(x) tolower(sub("\\.px$", "", sub("^.*/", "", x), ignore.case = TRUE))
  hits <- unique(tables[name(tables) == name(table_id)])
  if (length(hits) > 1) {
    stop("Table id '", table_id, "' matches several tables on ", base_url,
         ". Give the full path: ", paste0("\"", hits, "\"", collapse = ", "), ".", call. = FALSE)
  }
  if (length(hits) == 1) hits else table_id
}

.pxweb1_get_metadata <- function(table_id, base_url, lang, use_cache = TRUE) {
  table_id <- .pxweb1_resolve_id(table_id, base_url, lang)
  meta_url <- .pxweb1_url(base_url, lang, table_id)

  if (isTRUE(use_cache)) {
    cached <- .pxweb2_cache_get(meta_url)
    if (!is.null(cached)) return(cached)
  }

  resp <- .pxweb2_GET(meta_url, httr::accept_json())
  if (httr::status_code(resp) %in% c(400L, 404L)) {
    stop(
      "Table '", table_id, "' was not found on ", base_url, " in the language '", lang,
      "' (HTTP ", httr::status_code(resp), "). In PxWeb API v1 the table id is the ",
      "table's path below base_url, e.g. \"indikatorer/Indikatorm.px\".",
      call. = FALSE
    )
  }
  httr::stop_for_status(resp)

  raw <- .pxweb1_parse(resp)
  if (is.null(raw$variables)) {
    stop("'", table_id, "' on ", base_url, " is a folder, not a table.", call. = FALSE)
  }
  .pxweb2_cache_set(meta_url, .pxweb1_metadata(raw, table_id))
}

# The time of the table's latest update comes from its folder's listing.
# The listing gives local time without a time zone, so it can differ from the
# UTC time in the data by an hour or two.
.pxweb1_table_updated <- function(table_id, base_url, lang) {
  table_id <- .pxweb1_resolve_id(table_id, base_url, lang)
  folder <- sub("/?[^/]*$", "", table_id)
  id <- sub("^.*/", "", table_id)
  resp <- .pxweb2_GET(.pxweb1_url(base_url, lang, folder, folder = TRUE), httr::accept_json())
  httr::stop_for_status(resp)
  hit <- Filter(function(x) identical(x$id, id), .pxweb1_parse(resp))
  updated <- if (length(hit) > 0) hit[[1]]$updated %||% hit[[1]]$published
  if (is.null(updated)) NA_character_ else paste0(updated, "Z")
}

# One row per table, in the columns the v2 table list has. `paths` holds the
# folders as a one-element list with a data frame (id, label), as in v2.
.pxweb1_tables_tibble <- function(rows) {
  if (length(rows) == 0) {
    return(tibble::tibble(id = character(), label = character(), updated = character(),
                          firstPeriod = character(), lastPeriod = character(),
                          discontinued = logical(), subjectCode = character(), paths = list()))
  }
  chr <- function(name) vapply(rows, function(r) as.character(r[[name]] %||% NA_character_)[1], "")
  tibble::tibble(
    id = chr("id"), label = chr("label"), updated = chr("updated"),
    firstPeriod = NA_character_, lastPeriod = NA_character_, discontinued = FALSE,
    subjectCode = chr("subject"),
    paths = lapply(rows, function(r) {
      list(data.frame(id = r$folder_ids, label = r$folder_labels, stringsAsFactors = FALSE))
    })
  )
}

.pxweb1_table_row <- function(item, folder_ids, folder_labels) {
  list(
    id = paste(c(folder_ids, item$id), collapse = "/"),
    label = trimws(item$text %||% item$title %||% item$id),
    updated = item$updated %||% item$published,
    subject = if (length(folder_ids) > 0) folder_ids[1],
    folder_ids = as.character(folder_ids),
    folder_labels = as.character(folder_labels)
  )
}

# Reads every folder from base_url downwards, in the API's order. A folder that
# cannot be read is skipped with a warning, unless it is the first one.
.pxweb1_walk_tables <- function(base_url, lang, timeout_sec = 60) {
  rows <- list()
  failed <- character()
  stack <- list(list(ids = character(), labels = character()))
  first <- TRUE

  while (length(stack) > 0) {
    node <- stack[[1]]
    stack[[1]] <- NULL
    url <- .pxweb1_url(base_url, lang, paste(node$ids, collapse = "/"), folder = TRUE)
    resp <- tryCatch(.pxweb2_GET(url, httr::accept_json(), httr::timeout(timeout_sec)),
                     error = function(e) e)
    ok <- !inherits(resp, "error") && httr::status_code(resp) == 200
    if (!ok) {
      if (first) {
        stop("Could not read the table list from ", url, " (",
             if (inherits(resp, "error")) conditionMessage(resp) else
               paste0("HTTP ", httr::status_code(resp)), ").", call. = FALSE)
      }
      failed <- c(failed, paste(node$ids, collapse = "/"))
      next
    }
    first <- FALSE

    subfolders <- list()
    for (item in .pxweb1_parse(resp)) {
      id <- item$dbid %||% item$id
      if (is.null(id)) next
      if (!is.null(item$dbid) || identical(item$type, "l")) {
        subfolders[[length(subfolders) + 1]] <- list(
          ids = c(node$ids, id), labels = c(node$labels, trimws(item$text %||% id))
        )
      } else if (identical(item$type, "t")) {
        rows[[length(rows) + 1]] <- .pxweb1_table_row(item, node$ids, node$labels)
      }
    }
    stack <- c(subfolders, stack)
  }

  list(rows = rows, failed = failed)
}

# Sites with search can list every table at once with ?query=*&filter=*. The
# search gives the folders' ids but not their names, which are read from the
# folders above them, far fewer calls than reading every folder. NULL if the
# site has no such search (e.g. Skolverket), or base_url is not a database.
.pxweb1_search_all_tables <- function(base_url, lang, timeout_sec = 60) {
  if (grepl("/", .pxweb1_parts(base_url)$rest, fixed = TRUE) ||
      !nzchar(.pxweb1_parts(base_url)$rest)) {
    return(NULL)
  }
  found <- tryCatch({
    resp <- .pxweb2_GET(paste0(.pxweb1_url(base_url, lang), "?query=*&filter=*"),
                        httr::accept_json(), httr::timeout(timeout_sec))
    if (httr::status_code(resp) == 200) .pxweb1_parse(resp)
  }, error = function(e) NULL)
  if (!is.list(found) || length(found) == 0 || is.null(found[[1]]$path)) return(NULL)

  folders <- lapply(found, function(x) {
    p <- strsplit(as.character(x$path), "/", fixed = TRUE)[[1]]
    p[nzchar(p)]
  })
  # every folder that has a folder below it, starting with the database itself
  parents <- unique(unlist(lapply(folders, function(p) {
    vapply(seq_along(p) - 1L, function(k) paste(p[seq_len(k)], collapse = "/"), "")
  }), use.names = FALSE))
  labels <- list()
  for (parent in parents) {
    items <- tryCatch({
      resp <- .pxweb2_GET(.pxweb1_url(base_url, lang, parent, folder = TRUE), httr::accept_json(),
                          httr::timeout(timeout_sec))
      if (httr::status_code(resp) == 200) .pxweb1_parse(resp)
    }, error = function(e) NULL)
    for (item in items) {
      if (!is.null(item$id) && !is.null(item$text)) {
        labels[[paste(c(if (nzchar(parent)) parent, item$id), collapse = "/")]] <- trimws(item$text)
      }
    }
  }

  rows <- Map(function(item, p) {
    folder_labels <- vapply(seq_along(p), function(k) {
      labels[[paste(p[seq_len(k)], collapse = "/")]] %||% p[k]
    }, "")
    .pxweb1_table_row(item, p, folder_labels)
  }, found, folders)
  list(rows = unname(rows), failed = character())
}

# All tables of the site, kept in the session cache: reading the folders of a
# large site takes minutes. A list with folders missing is not kept.
.pxweb1_all_tables <- function(base_url, lang, timeout_sec = 60) {
  key <- paste0("tables:", .pxweb1_url(base_url, lang))
  cached <- .pxweb2_cache_get(key)
  if (!is.null(cached)) return(cached)

  res <- .pxweb1_search_all_tables(base_url, lang, timeout_sec)
  if (is.null(res)) {
    message("PxWeb API v1 has no list of tables: reading every folder of ", base_url,
            ". This can take a few minutes. The result is kept for this session.")
    res <- .pxweb1_walk_tables(base_url, lang, timeout_sec)
  }

  tables <- .pxweb1_tables_tibble(res$rows)
  if (length(res$failed) > 0) {
    warning(length(res$failed), " folder(s) on ", base_url, " could not be read, so the ",
            "table list is incomplete: ", paste(utils::head(res$failed, 5), collapse = ", "),
            if (length(res$failed) > 5) ", ...", call. = FALSE)
    return(tables)
  }
  .pxweb2_cache_set(key, tables)
}

# v1 counterpart of the /tables endpoint. The search and the date are applied
# here, since v1 has no such parameters on the table list.
.pxweb1_search_tables <- function(query, base_url, lang, pastDays = NULL, timeout_sec = 60) {
  .pxweb1_filter_tables(.pxweb1_all_tables(base_url, lang, timeout_sec), query, pastDays)
}

.pxweb1_filter_tables <- function(tables, query = NULL, pastDays = NULL) {
  if (!is.null(query) && nzchar(trimws(query))) {
    hay <- tolower(paste(tables$id, tables$label))
    for (w in strsplit(tolower(trimws(query)), "\\s+")[[1]]) {
      tables <- tables[grepl(w, hay, fixed = TRUE), , drop = FALSE]
      hay <- tolower(paste(tables$id, tables$label))
    }
  }
  if (!is.null(pastDays)) {
    updated <- as.Date(substr(tables$updated, 1, 10))
    tables <- tables[!is.na(updated) & updated >= Sys.Date() - pastDays, , drop = FALSE]
  }
  tables
}

# The request body of a query_list in v2 form. "*" and top(n) have filters of
# their own; v1 has no bottom(n), so it becomes the first n codes of the table.
.pxweb1_post_body <- function(body, valid_values_list, output_format = "json-stat2") {
  query <- lapply(body$selection, function(s) {
    vals <- as.character(unlist(s$valueCodes, use.names = FALSE))
    n <- if (length(vals) == 1) {
      m <- regmatches(vals, regexec("^\\s*(top|bottom)\\s*\\(\\s*(\\d+)\\s*\\)\\s*$", vals,
                                    ignore.case = TRUE))[[1]]
      if (length(m) == 3) list(kind = tolower(m[2]), n = m[3])
    }
    selection <- if (identical(vals, "*")) {
      list(filter = "all", values = list("*"))
    } else if (identical(n$kind, "top")) {
      list(filter = "top", values = list(n$n))
    } else if (identical(n$kind, "bottom")) {
      codes <- valid_values_list[[s$variableCode]]$code
      list(filter = "item", values = as.list(utils::head(codes, as.integer(n$n))))
    } else {
      list(filter = "item", values = as.list(vals))
    }
    list(code = s$variableCode, selection = selection)
  })
  list(query = query, response = list(format = output_format))
}

# Aggregations need code lists, which v1 and Statistics Denmark do not have
.pxweb1_no_aggregations <- function(include_aggregations, api = "PxWeb API v1") {
  if (!isFALSE(include_aggregations) && !identical(include_aggregations, "none")) {
    warning(api, " has no code lists, so include_aggregations is ignored. ",
            "Only the values in the table itself can be fetched.", call. = FALSE)
  }
  "none"
}

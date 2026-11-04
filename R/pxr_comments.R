# Comments (footnotes) of tables, in one form whatever the API.
#
# Where they are:
#   - API v2: in the metadata, as note on the table, a variable and a value's
#     category. The notes that must be shown are listed by their position, from 0,
#     in extension$noteMandatory and extension$categoryNoteMandatory.
#   - API v1: not in the metadata, only in the answer to a data request. Notes on
#     the table and variables come with format json-stat2 at some sites and only with
#     format px at others, and notes on values with format json, for the values that
#     were asked for. So every value of one variable at a time is asked for, with the
#     first value of the others.
#   - Statistics Denmark: footnote in tableinfo, for the whole table only.

.pxr_no_comments <- function() {
  tibble::tibble(tabellid = character(), niva = character(), var_kod = character(),
                 var_namn = character(), varde_kod = character(), varde_namn = character(),
                 kommentar = character(), obligatorisk = logical())
}

# One row per text. Empty texts are dropped, and line ends made the same.
.pxr_comment_rows <- function(niva, texts, var_kod = NA_character_, var_namn = NA_character_,
                              varde_kod = NA_character_, varde_namn = NA_character_,
                              obligatorisk = NA) {
  texts <- trimws(gsub("\r\n?", "\n", as.character(unlist(texts, use.names = FALSE))))
  keep <- nzchar(texts)
  if (!any(keep)) return(.pxr_no_comments())
  tibble::tibble(tabellid = NA_character_, niva = niva, var_kod = var_kod, var_namn = var_namn,
                 varde_kod = varde_kod, varde_namn = varde_namn, kommentar = texts[keep],
                 obligatorisk = rep_len(obligatorisk, length(texts))[keep])
}

# TRUE for the notes whose position, from 0, is in `mandatory`. `absent` is what is
# known if the answer does not have `mandatory` at all.
.pxr_note_mandatory <- function(notes, mandatory, absent) {
  if (is.null(mandatory)) return(rep(absent, length(unlist(notes))))
  mandatory <- mandatory[vapply(mandatory, isTRUE, TRUE)]
  (seq_along(unlist(notes)) - 1L) %in% suppressWarnings(as.integer(names(mandatory)))
}

# Every note in JSON-stat 2: the table's, and per variable its own and its values'
.pxr_comments_jsonstat <- function(x, absent = FALSE) {
  rows <- list(.pxr_comment_rows(
    "tabell", x$note, obligatorisk = .pxr_note_mandatory(x$note, x$extension$noteMandatory, absent)
  ))
  dims <- .pxr_dims(x)
  for (code in names(dims)) {
    d <- dims[[code]]
    name <- as.character(d$label %||% code)
    rows[[length(rows) + 1]] <- .pxr_comment_rows(
      "variabel", d$note, code, name,
      obligatorisk = .pxr_note_mandatory(d$note, d$extension$noteMandatory, absent)
    )
    for (value in names(d$category$note)) {
      note <- d$category$note[[value]]
      rows[[length(rows) + 1]] <- .pxr_comment_rows(
        "värde", note, code, name, value, as.character(d$category$label[[value]] %||% NA_character_),
        .pxr_note_mandatory(note, d$extension$categoryNoteMandatory[[value]], absent)
      )
    }
  }
  dplyr::bind_rows(rows)
}

# The answer in format json: comments per value, and per column for the variables.
# A column that is not a variable is the table's content, so its comment is the table's.
.pxr_comments_json <- function(x, metadata) {
  dims <- metadata$dimension
  rows <- lapply(x$columns, function(col) {
    code <- as.character(col$code)
    if (code %in% names(dims)) {
      .pxr_comment_rows("variabel", col$comment, code, as.character(dims[[code]]$label %||% code))
    } else {
      .pxr_comment_rows("tabell", col$comment)
    }
  })
  rows <- c(rows, lapply(x$comments, function(cm) {
    code <- as.character(cm$variable)
    value <- as.character(cm$value)
    .pxr_comment_rows("värde", cm$comment, code, as.character(dims[[code]]$label %||% code),
                      value, as.character(dims[[code]]$category$label[[value]] %||% NA_character_))
  }))
  dplyr::bind_rows(rows)
}

# A px file as text. The header says which code page it has, but also a site that
# answers in UTF-8 can say another, so UTF-8 is used whenever the bytes are valid UTF-8.
.pxweb1_px_text <- function(bytes) {
  if (identical(utils::head(bytes, 3), as.raw(c(0xef, 0xbb, 0xbf)))) bytes <- bytes[-(1:3)]
  txt <- rawToChar(bytes)
  if (validUTF8(txt)) {
    Encoding(txt) <- "UTF-8"
    return(txt)
  }
  page <- regmatches(txt, regexec('CODEPAGE="([^"]+)"', txt, useBytes = TRUE))[[1]][2]
  out <- if (!is.na(page)) suppressWarnings(iconv(txt, page, "UTF-8"))
  if (is.null(out) || is.na(out)) iconv(txt, "windows-1252", "UTF-8", sub = "?") else out
}

# NOTE and NOTEX of a px file, as a tibble with one row per note: on the table, or on
# the variable in NOTE("variable"). NOTEX is what must be shown. A value runs over
# several quoted lines; ## divides notes, and # is a line break.
.pxweb1_px_notes <- function(txt) {
  header <- sub("(?s)[\r\n]+DATA=.*$", "", txt, perl = TRUE)
  pattern <- '(?m)^(NOTEX?)(?:\\("([^"]*)"\\))?=((?:\\s*"[^"]*")+)\\s*;'
  items <- regmatches(header, gregexpr(pattern, header, perl = TRUE))[[1]]
  rows <- lapply(items, function(item) {
    m <- regmatches(item, regexec(pattern, item, perl = TRUE))[[1]]
    text <- gsub('^\\s*"|"\\s*$', "", gsub('"\\s*"', "", m[4], perl = TRUE))
    notes <- gsub("#", "\n", strsplit(text, "##", fixed = TRUE)[[1]], fixed = TRUE)
    tibble::tibble(variable = m[3], kommentar = notes, obligatorisk = m[2] == "NOTEX")
  })
  dplyr::bind_rows(tibble::tibble(variable = character(), kommentar = character(),
                                  obligatorisk = logical()), rows)
}

# The notes from a px file that json-stat2 did not have, for the table and per variable
.pxweb1_comments_px <- function(notes, have, metadata) {
  labels <- vapply(metadata$dimension, function(d) as.character(d$label %||% NA_character_), "")
  rows <- lapply(split(notes, notes$variable, drop = TRUE), function(d) {
    code <- names(labels)[match(d$variable[1], labels)]
    if (is.na(code)) return(NULL)
    if (any(have$niva == "variabel" & have$var_kod %in% code)) return(NULL)
    .pxr_comment_rows("variabel", d$kommentar, code, d$variable[1], obligatorisk = d$obligatorisk)
  })
  hela <- notes[notes$variable == "", , drop = FALSE]
  c(if (!any(have$niva == "tabell")) {
    list(.pxr_comment_rows("tabell", hela$kommentar, obligatorisk = hela$obligatorisk))
  }, unname(rows))
}

.pxweb1_get_comments <- function(table_id, base_url, lang) {
  # sets the pace of calls that the site allows
  limit <- .pxweb2_get_config(base_url)$maxDataCells
  metadata <- .pxweb1_get_metadata(table_id, base_url, lang)
  url <- .pxweb1_url(base_url, lang, metadata$extension$px$tableid)
  values <- lapply(metadata$dimension, function(d) .pxr_dim_values(d)$code)
  first <- lapply(values, utils::head, 1)

  failed <- integer()
  post <- function(selected, format) {
    query <- unname(Map(function(code, vals) {
      list(code = code, selection = list(filter = "item", values = as.list(vals)))
    }, names(selected), selected))
    .pxweb2_POST(
      url, body = jsonlite::toJSON(list(query = query, response = list(format = format)), auto_unbox = TRUE),
      encode = "raw", httr::content_type_json(), httr::accept_json()
    )
  }
  ask <- function(selected, format) {
    resp <- post(selected, format)
    if (httr::status_code(resp) != 200) {
      failed <<- c(failed, httr::status_code(resp))
      return(NULL)
    }
    .pxweb1_parse(resp)
  }

  answer <- ask(first, "json-stat2")
  rows <- list(if (!is.null(answer)) .pxr_comments_jsonstat(answer, absent = NA))
  # not every site has format px, and json-stat2 and json are then all there is
  resp <- post(first, "px")
  if (httr::status_code(resp) == 200) {
    notes <- tryCatch(.pxweb1_px_notes(.pxweb1_px_text(httr::content(resp, "raw"))), error = function(e) NULL)
    if (!is.null(notes)) {
      rows <- c(rows, .pxweb1_comments_px(notes, dplyr::bind_rows(.pxr_no_comments(), rows), metadata))
    }
  }
  for (code in names(values)) {
    vals <- values[[code]]
    size <- if (is.numeric(limit) && length(limit) == 1 && limit >= 1) limit else length(vals)
    for (part in split(vals, ceiling(seq_along(vals) / size))) {
      selected <- first
      selected[[code]] <- part
      answer <- ask(selected, "json")
      if (!is.null(answer)) rows[[length(rows) + 1]] <- .pxr_comments_json(answer, metadata)
    }
  }
  if (length(failed) > 0) {
    warning("Some comments of table '", table_id, "' could not be read (HTTP ",
            paste(unique(failed), collapse = ", "), ").", call. = FALSE)
  }
  out <- dplyr::bind_rows(.pxr_no_comments(), rows)
  # the same comment comes with every answer; the first has what is known of it
  out <- out[!duplicated(out[c("niva", "var_kod", "varde_kod", "kommentar")]), , drop = FALSE]
  list(table = metadata$extension$px$tableid, rows = out, metadata = metadata)
}

# Table, then per variable in the table's order its own comments and its values'
.pxr_comments_sort <- function(rows, metadata) {
  codes <- names(.pxr_dims(metadata))
  rows[order(ifelse(is.na(rows$var_kod), 0L, match(rows$var_kod, codes)), rows$niva == "värde"), , drop = FALSE]
}

.pxr_comments_one <- function(tab, base_url, lang) {
  res <- if (.pxweb1_is_url(base_url)) {
    .pxweb1_get_comments(tab, base_url, lang)
  } else {
    metadata <- pxweb2_get_metadata(tab, base_url = base_url, lang = lang)
    list(table = tab, rows = dplyr::bind_rows(.pxr_no_comments(), .pxr_comments_jsonstat(metadata)),
         metadata = metadata)
  }
  rows <- .pxr_comments_sort(res$rows, res$metadata)
  rows$tabellid <- rep(res$table, nrow(rows))
  rows
}

#' Comments of a table
#'
#' Fetches the comments (footnotes) that a provider has written for a table: on
#' the whole table, on a variable or on a value of a variable.
#'
#' Where the comments come from depends on the API, and the function finds them
#' in the same way for every provider. In PxWeb API v2 they are in the table's
#' metadata, which takes one call. In PxWeb API v1 they come only with data, so the
#' function makes one small data request per variable and two more, a few calls per
#' table, since some sites give the comments on the table in format px only. Statistics
#' Denmark has comments on the whole table only. The text is as the
#' provider wrote it, and can have links in HTML or Markdown.
#'
#' @inheritParams pxr_get_labels
#' @param tab Table id, e.g. `"TAB638"`, or a vector of table ids, e.g.
#'   `c("TAB5557", "TAB638")`. In PxWeb API v1 the table's path below the provider's
#'   URL, e.g. `"indikatorer/Indikatorm.px"`, or an id without folders, see
#'   [pxr_get_labels()]. Obligatorisk.
#'
#' @return A `tibble` with one row per comment, in the table's order: the table's
#'   comments first, then those of each variable and its values. A message says
#'   which tables have no comments, and the result is empty if none has any. The
#'   columns are:
#'   * `tabellid`: the table's id. In PxWeb API v1 always the whole path.
#'   * `niva`: what the comment applies to, `"tabell"`, `"variabel"` or `"värde"`.
#'   * `var_kod`, `var_namn`: the variable's code and label; `NA` for a comment on
#'     the table.
#'   * `varde_kod`, `varde_namn`: the value's code and label; `NA` unless the comment
#'     is on a value.
#'   * `kommentar`: the text of the comment.
#'   * `obligatorisk`: `TRUE` if the provider says that the comment must be shown
#'     with the figures, `FALSE` if it is not required, and `NA` if the API does not
#'     tell, which is common in PxWeb API v1.
#' @seealso [pxr_get_all_labels()], [pxr_get_all_tabs()]
#' @export
#' @example inst/examples/pxr_get_comments.R
pxr_get_comments <- function(provider = "scb", tab, lang = NULL) {
  if (missing(tab) || is.null(tab) || !is.character(tab) || length(tab) == 0 || anyNA(tab)) {
    stop("tab must be one or more table ids as text", call. = FALSE)
  }
  base_url <- .pxr_tables_url(provider)
  lang <- .pxweb2_resolve_lang(lang, base_url)
  tab <- unique(tab)
  rows <- lapply(tab, .pxr_comments_one, base_url = base_url, lang = lang)
  empty <- tab[vapply(rows, nrow, 1L) == 0]
  if (length(empty) > 0) {
    message("No comments in: ", paste(empty, collapse = ", "), ".")
  }
  dplyr::bind_rows(.pxr_no_comments(), rows)
}

# Tabellkatalog och ämnesträd
#
# PxWeb API v2 har ingen navigeringsendpoint, men varje tabell i
# /tables-listan har fältet `paths` - en eller flera sökvägar av mappar
# (ämnesområde > underområde > ...). Trädet byggs därför från hela
# tabellistan, som hämtas en gång och cachas på disk. API v1 saknar
# tabellista, men pxweb2_search_tables() ger samma form genom att läsa mapparna.

# En cachefil per webbplats och språk, t.ex. tabeller_statistikdatabasen.scb.se_api_v2_sv.rds
px_cache_file <- function(base_url = NULL, lang = NULL) {
  custom <- getOption("pxweb2rgui.cache_file")
  if (!is.null(custom) && is.null(base_url) && is.null(lang)) return(custom)
  key <- if (is.null(base_url) && is.null(lang)) "scb" else {
    paste(c(px_site_slug(base_url %||% ""), lang), collapse = "_")
  }
  if (!is.null(custom)) return(sub("(\\.rds)?$", paste0("_", key, ".rds"), custom))
  dir <- tools::R_user_dir("pxweb2rgui", which = "cache")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file.path(dir, paste0("tabeller_", key, ".rds"))
}

px_site_slug <- function(base_url) {
  x <- sub("^https?://", "", base_url)
  x <- sub("/?tables/?$", "", x)
  x <- gsub("[^A-Za-z0-9.-]+", "_", x)
  sub("_+$", "", x)
}

# Hämtar alla tabeller (inkl. avslutade, de filtreras bort i appen vid behov).
# Använder cachen om den är yngre än max_age_hours.
px_fetch_catalogue <- function(backend = px_backend(),
                               force = FALSE,
                               max_age_hours = 24,
                               base_url = NULL,
                               lang = NULL,
                               cache_file = px_cache_file(base_url, lang)) {
  if (!force && file.exists(cache_file)) {
    age <- difftime(Sys.time(), file.mtime(cache_file), units = "hours")
    if (age < max_age_hours) {
      cached <- tryCatch(readRDS(cache_file), error = function(e) NULL)
      if (!is.null(cached)) return(cached)
    }
  }

  args <- list(query = NULL, includeDiscontinued = "true")
  if (!is.null(base_url)) args$base_url <- base_url
  if (!is.null(lang)) args$lang <- lang
  raw <- do.call(backend$search_tables, args)
  catalogue <- px_parse_catalogue(raw)
  catalogue$fetched <- Sys.time()
  catalogue$base_url <- base_url
  catalogue$lang <- lang

  tryCatch(saveRDS(catalogue, cache_file), error = function(e) NULL)
  catalogue
}

# Gör om svaret från pxweb2_search_tables() till två tabeller:
#   tables: en rad per tabell
#   paths:  en rad per (tabell, sökväg, nivå) med mappens nyckel och namn
px_parse_catalogue <- function(raw) {
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  n <- nrow(raw)
  col <- function(name, default = NA) {
    if (name %in% names(raw)) raw[[name]] else rep(default, n)
  }

  tables <- data.frame(
    id           = as.character(col("id")),
    label        = as.character(col("label")),
    first_period = as.character(col("firstPeriod")),
    last_period  = as.character(col("lastPeriod")),
    updated      = substr(as.character(col("updated")), 1, 10),
    discontinued = as.logical(col("discontinued", FALSE)),
    subject_code = as.character(col("subjectCode")),
    stringsAsFactors = FALSE
  )
  tables$discontinued[is.na(tables$discontinued)] <- FALSE

  paths_col <- col("paths", NULL)
  rows <- lapply(seq_len(n), function(i) {
    p <- if (is.null(paths_col)) NULL else paths_col[[i]]
    px_paths_rows(tables$id[i], px_normalise_paths(p))
  })
  paths <- do.call(rbind, c(list(px_empty_paths()), rows))

  # Tabeller utan sökväg hamnar under en egen gren så att de inte försvinner
  missing <- setdiff(tables$id, paths$table_id)
  if (length(missing) > 0) {
    paths <- rbind(paths, data.frame(
      table_id = missing, path_no = 1L, depth = 1L,
      key = "_utan_amne", label = "(Utan ämnesområde)", sort_code = NA_character_,
      stringsAsFactors = FALSE
    ))
  }

  list(tables = tables, paths = paths)
}

px_empty_paths <- function() {
  data.frame(table_id = character(), path_no = integer(), depth = integer(),
             key = character(), label = character(), sort_code = character(),
             stringsAsFactors = FALSE)
}

# `paths` för en tabell kan, beroende på hur jsonlite förenklat svaret, vara
# en lista av data.frames, en enskild data.frame eller en lista av listor.
# Returnerar alltid en lista av data.frames med kolumnerna id och label.
px_normalise_paths <- function(p) {
  if (is.null(p) || length(p) == 0) return(list())
  if (is.data.frame(p)) p <- list(p)

  # en ensam sökväg som lista av element: list(list(id=, label=), ...)
  if (is.list(p) && !is.null(p[[1]]$id) && !is.data.frame(p[[1]]) &&
      length(p[[1]]$id) == 1 && !is.list(p[[1]]$id)) {
    p <- list(p)
  }

  out <- lapply(p, function(one) {
    if (is.data.frame(one)) {
      df <- one
    } else {
      df <- data.frame(
        id       = vapply(one, function(e) as.character(e$id %||% NA), ""),
        label    = vapply(one, function(e) as.character(e$label %||% NA), ""),
        sortCode = vapply(one, function(e) as.character(e$sortCode %||% NA), ""),
        stringsAsFactors = FALSE
      )
    }
    if (!all(c("id", "label") %in% names(df)) || nrow(df) == 0) return(NULL)
    data.frame(
      id        = as.character(df$id),
      label     = as.character(df$label),
      sort_code = if ("sortCode" %in% names(df)) as.character(df$sortCode) else NA_character_,
      stringsAsFactors = FALSE
    )
  })
  Filter(Negate(is.null), out)
}

px_paths_rows <- function(table_id, paths) {
  if (length(paths) == 0) return(px_empty_paths())
  do.call(rbind, lapply(seq_along(paths), function(j) {
    df <- paths[[j]]
    data.frame(
      table_id = table_id,
      path_no  = j,
      depth    = seq_len(nrow(df)),
      key      = vapply(seq_len(nrow(df)),
                        function(k) paste(df$id[seq_len(k)], collapse = "/"), ""),
      label    = df$label,
      sort_code = df$sort_code,
      stringsAsFactors = FALSE
    )
  }))
}

# Filtrerar tabellerna på fritext (alla ord måste finnas i id eller namn)
# och på om avslutade tabeller ska visas.
px_filter_tables <- function(tables, text = "", include_discontinued = FALSE) {
  keep <- rep(TRUE, nrow(tables))
  if (!include_discontinued) keep <- keep & !tables$discontinued
  words <- strsplit(tolower(trimws(text %||% "")), "\\s+")[[1]]
  words <- words[nzchar(words)]
  if (length(words) > 0) {
    hay <- tolower(paste(tables$id, tables$label))
    for (w in words) keep <- keep & grepl(w, hay, fixed = TRUE)
  }
  tables[keep, , drop = FALSE]
}

# Bygger trädet som en nästlad lista. Varje nod har:
#   key, label, children (lista av noder), tables (tabell-id:n direkt i noden),
#   n (antal tabeller i noden och alla undernoder)
# Mappar sorteras på sortCode om API:et skickar med det och annars, liksom
# tabellerna, i den ordning de kommer i API:ets lista.
px_build_tree <- function(paths, table_ids = NULL) {
  if (!is.null(table_ids)) paths <- paths[paths$table_id %in% table_ids, , drop = FALSE]
  paths$row <- seq_len(nrow(paths))

  # mappar: en rad per unik nyckel, med förälder
  folders <- paths[!duplicated(paths$key), c("key", "label", "depth", "sort_code", "row")]
  folders <- folders[order(folders$sort_code, folders$row, na.last = TRUE), ]
  folders$parent <- ifelse(folders$depth == 1, px_root_key,
                           sub("/[^/]*$", "", folders$key))

  # var ligger varje tabell? i den djupaste mappen av varje sökväg
  table_order <- match(paths$table_id, unique(paths$table_id))
  last <- paths[order(table_order, paths$path_no, -paths$depth), ]
  last <- last[!duplicated(last[, c("table_id", "path_no")]), ]
  last <- last[!duplicated(last[, c("table_id", "key")]), ]
  tables_by_key <- split(last$table_id, last$key)
  children_by_parent <- split(folders$key, folders$parent)
  label_by_key <- stats::setNames(folders$label, folders$key)

  make_node <- function(key) {
    kids <- lapply(children_by_parent[[key]] %||% character(), make_node)
    tabs <- unique(tables_by_key[[key]] %||% character())
    n <- length(unique(c(tabs, unlist(lapply(kids, `[[`, "all_ids")))))
    list(
      key = key,
      label = if (key == px_root_key) "" else label_by_key[[key]],
      children = kids,
      tables = tabs,
      all_ids = unique(c(tabs, unlist(lapply(kids, `[[`, "all_ids")))),
      n = n
    )
  }
  make_node(px_root_key)
}

# Nyckel för trädets rot (en tom sträng fungerar inte som listnamn i R)
px_root_key <- "<rot>"

# Antal ämnesområden (nivå 1), alla mappar och tabeller - för statusraden
px_tree_stats <- function(tree) {
  count_folders <- function(node) {
    length(node$children) + sum(vapply(node$children, count_folders, 0))
  }
  list(
    subjects = length(tree$children),
    folders  = count_folders(tree),
    tables   = tree$n
  )
}

# Ritar trädet som nästlade <details>-element. Tabellerna är länkar som
# skickar tabell-id:t till Shiny (input$vald_tabell) via JavaScript i app.R.
px_tree_html <- function(tree, tables, open = FALSE, selected = NULL) {
  info <- tables[!duplicated(tables$id), , drop = FALSE]
  rownames(info) <- info$id

  table_item <- function(id) {
    r <- info[id, ]
    period <- if (!is.na(r$first_period) && !is.na(r$last_period)) {
      paste0(r$first_period, "–", r$last_period)
    } else ""
    cls <- c("px-table", if (id %in% selected) "px-selected",
             if (isTRUE(r$discontinued)) "px-discontinued")
    shiny::tags$li(
      shiny::tags$a(
        href = "#", class = paste(cls, collapse = " "), `data-id` = id,
        # i API v1 är id:t hela sökvägen, som trädet redan visar
        shiny::tags$span(class = "px-table-id", sub("^.*/", "", id)),
        shiny::tags$span(class = "px-table-label", r$label)
      ),
      if (nzchar(period)) shiny::tags$span(class = "px-period", period),
      if (isTRUE(r$discontinued)) shiny::tags$span(class = "px-badge", "avslutad")
    )
  }

  node_html <- function(node) {
    body <- shiny::tagList(
      lapply(node$children, node_html),
      if (length(node$tables) > 0) {
        shiny::tags$ul(class = "px-tables", lapply(node$tables, table_item))
      }
    )
    if (node$key == px_root_key) return(body)
    shiny::tags$details(
      class = "px-node", `data-key` = node$key, open = if (open) NA else NULL,
      shiny::tags$summary(
        shiny::tags$span(class = "px-folder", node$label),
        shiny::tags$span(class = "px-count", node$n)
      ),
      shiny::div(class = "px-children", body)
    )
  }

  shiny::div(class = "px-tree", node_html(tree))
}

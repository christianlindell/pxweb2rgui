# Bygger frågan (query-listan) till pxr_pxweb2_get_data() och motsvarande R-kod
#
# Varje variabel har en lista med valda värden, som på SCB:s webbplats:
#   alla värden valda         -> "*"
#   inga värden, eliminerbar  -> NA (variabeln utelämnas)
#   inga värden, obligatorisk -> problem, frågan kan inte köras
#   "senaste perioden" ikryssad (tid) -> "9999" (pxweb2r:s latest_period_code)
#   annars                    -> de valda koderna
#
# Med en annan indelning (kodlista) än tabellens egen gäller:
#   värdemängd (vs_...)   -> koderna finns i tabellen, så de skickas som vanligt
#                            men alltid som en uttrycklig lista, inte "*"
#   aggregering (agg_...) -> koderna är grupper; pxweb2r behöver
#                            include_aggregations = c(variabel = "agg_...")

# variables: från pxweb2_get_variables(); values: namngiven lista med
# värdetabeller (kolumnen code) för den indelning som visas;
# selections: variabelkod -> list(values =, latest =, codelist =, codelist_type =)
# Returnerar list(query = , include_aggregations = , problems = ).
px_build_query <- function(variables, values, selections) {
  problems <- character()
  query <- list()
  aggregations <- character()
  for (i in seq_len(nrow(variables))) {
    code <- variables$code[i]
    s <- selections[[code]] %||% list()
    chosen <- as.character(s$values %||% character())
    all_codes <- values[[code]]$code %||% character()
    codelist <- s$codelist %||% ""
    if (nzchar(codelist) && identical(tolower(s$codelist_type %||% ""), "aggregation") &&
        length(chosen) > 0) {
      aggregations[[code]] <- codelist
    }

    query[[code]] <- if (isTRUE(s$latest)) {
      "9999"
    } else if (length(chosen) == 0) {
      if (!isTRUE(variables$elimination[i])) {
        problems <- c(problems, paste0("Välj minst ett värde för ", variables$label[i], "."))
      }
      NA
    } else if (!nzchar(codelist) && length(all_codes) > 1 && setequal(chosen, all_codes)) {
      "*"
    } else {
      all_codes[all_codes %in% chosen]   # listans ordning
    }
  }
  list(query = query,
       include_aggregations = if (length(aggregations) > 0) aggregations else NULL,
       problems = problems)
}

# Variablerna som knappen "Välj alla värden" gäller: alla utom värdevariablerna
px_mark_all_positions <- function(variables) {
  which(!variables$role %in% "contents")
}

# Varning för mycket stora urval. Hämtningen stoppas inte.
px_cell_warning <- function(cells, limit = 1e6) {
  if (cells > limit) {
    paste("Varning: urvalet är större än 1 miljon celler. Hämtningen kan ta lång tid",
          "och belasta statistikleverantörens server.")
  }
}

# Ungefärligt antal celler som frågan ger (för att visa storleken)
px_cell_count <- function(variables, query) {
  sizes <- vapply(seq_len(nrow(variables)), function(i) {
    v <- query[[variables$code[i]]]
    size <- variables$size[i]
    if (is.null(v) || identical(v, "*")) return(as.numeric(size))
    if (length(v) == 1 && is.na(v)) return(1)
    if (identical(v, "9999")) return(1)
    if (length(v) == 1 && grepl("^top\\((\\d+)\\)$", v)) {
      return(min(size, as.numeric(sub("^top\\((\\d+)\\)$", "\\1", v))))
    }
    as.numeric(length(v))
  }, 0)
  prod(sizes)
}

# Namn som jämförs mellan värden: små bokstäver, utan omgivande blanksteg. Saknas
# namnet gäller koden.
px_value_key <- function(d) {
  k <- tolower(trimws(as.character(d$label)))
  ifelse(is.na(k) | k == "", as.character(d$code), k)
}

# Lägger till koden efter namn som annars skulle stå på flera rader
px_disambiguate_labels <- function(vals) {
  key <- px_value_key(vals)
  dubbla <- key %in% key[duplicated(key)]
  if (any(dubbla)) vals$label[dubbla] <- paste0(vals$label[dubbla], " (", vals$code[dubbla], ")")
  vals
}

# Listan för en variabel som flera tabeller har. per_table: tabell-id -> värdetabell.
# Samma namn betyder inte alltid samma tal (FA1100 och FL1100 heter båda "monetärt
# guld" i SCB:s TAB1194), och samma kod inte alltid samma sak (kod 50 är olika månader
# i två tabeller hos Jordbruksverket). Därför gäller:
# * Samma kod och samma namn blir en rad.
# * Olika koder med samma namn blir en rad bara om varje tabell har högst en kod med
#   namnet, så att ingen tabell får samma tal två gånger (`alias` ger koderna). Annars
#   blir de egna rader, med koden i namnet.
# * Har samma kod olika namn i olika tabeller väljs värdena på namn: raden och frågan
#   innehåller namnet, och varje tabell översätter det till sin egen kod. Då går bara
#   en kod per namn och tabell att välja, och `dropped` räknar övriga.
# * Regioner slås inte ihop på namn, eftersom två orter kan heta likadant, och koden
#   gäller om bara enstaka namn skiljer sig (t.ex. stavning).
# Returnerar list(values, alias, to_row = tabellens kod -> radens värde, dropped).
px_merge_values <- function(per_table, role) {
  ids <- names(per_table)
  all_vals <- do.call(rbind, lapply(ids, function(id) {
    d <- as.data.frame(per_table[[id]])
    data.frame(table = id, code = as.character(d$code), label = as.character(d$label),
               key = px_value_key(d), stringsAsFactors = FALSE)
  }))
  is_geo <- identical(role, "geo")

  # har samma kod olika namn, eller är en kod namnet på en annan kod i en annan tabell?
  per_code <- split(all_vals, all_vals$code)
  delade <- per_code[vapply(per_code, function(x) length(unique(x$table)) > 1, TRUE)]
  skiljer <- sum(vapply(delade, function(x) length(unique(x$key)) > 1, TRUE))
  by_name <- if (is_geo) {
    length(delade) > 0 && skiljer / length(delade) >= 0.5
  } else {
    kandidater <- which(all_vals$key != all_vals$code & all_vals$key %in% all_vals$code)
    skiljer > 0 || any(vapply(kandidater, function(i) {
      any(all_vals$code == all_vals$key[i] & all_vals$table != all_vals$table[i])
    }, TRUE))
  }

  rows <- list()
  alias <- list()
  to_row <- list()
  dropped <- 0L
  add_row <- function(value, label, tables) {
    rows[[length(rows) + 1]] <<- data.frame(code = value, label = label,
                                            n = length(unique(tables)),
                                            tables = paste(unique(tables), collapse = "\r"),
                                            stringsAsFactors = FALSE)
  }

  if (by_name) {
    for (k in unique(all_vals$key)) {
      x <- all_vals[all_vals$key == k, , drop = FALSE]
      value <- if (is.na(x$label[1]) || !nzchar(trimws(x$label[1]))) x$code[1] else x$label[1]
      add_row(value, value, x$table)
      dropped <- dropped + nrow(x) - length(unique(x$table))
      for (i in seq_len(nrow(x))) to_row[[x$table[i]]][[x$code[i]]] <- value
    }
  } else {
    # en region följer koden, även om stavningen skiljer sig
    if (is_geo) all_vals$key <- all_vals$key[match(all_vals$code, all_vals$code)]
    per_key <- split(all_vals, factor(all_vals$key, levels = unique(all_vals$key)))
    for (x in per_key) {
      koder <- unique(x$code)
      en_per_tabell <- !anyDuplicated(x$table)
      if (length(koder) > 1 && en_per_tabell && !is_geo) {
        alias[[koder[1]]] <- koder
        add_row(koder[1], x$label[1], x$table)
        for (i in seq_len(nrow(x))) to_row[[x$table[i]]][[x$code[i]]] <- koder[1]
      } else {
        for (k in koder) {
          y <- x[x$code == k, , drop = FALSE]
          add_row(k, y$label[1], y$table)
          for (id in y$table) to_row[[id]][[k]] <- k
        }
      }
    }
    # koderna i den ordning tabellerna har dem
    ordning <- unique(all_vals$code)
    rows <- rows[order(match(vapply(rows, function(r) r$code, ""), ordning))]
  }

  vals <- do.call(rbind, rows)
  vals <- px_disambiguate_labels(vals)
  vals$common <- vals$n == length(ids)
  for (i in which(!vals$common)) {
    har <- strsplit(vals$tables[i], "\r", fixed = TRUE)[[1]]
    lab <- if (is.na(vals$label[i])) vals$code[i] else vals$label[i]
    vals$label[i] <- paste0(lab, " (bara ", paste(sub("^.*/", "", har), collapse = ", "), ")")
  }
  # tiden sorteras på namn: koden kan vara ett löpnummer
  if (identical(role, "time")) vals <- vals[order(px_value_key(vals)), , drop = FALSE]
  rownames(vals) <- NULL
  list(values = vals[, c("code", "label", "common"), drop = FALSE], alias = alias,
       to_row = to_row, dropped = dropped)
}

# Frågans rader -> koderna att fråga efter. "*" gäller varje tabells alla värden.
px_expand_aliases <- function(query, aliases, indelade = character()) {
  for (code in setdiff(names(aliases), indelade)) {
    v <- query[[code]]
    if (is.null(v) || (length(v) == 1 && is.na(v)) || identical(v, "*")) next
    query[[code]] <- unique(unlist(lapply(v, function(x) aliases[[code]][[x]] %||% x),
                                   use.names = FALSE))
  }
  query
}

# Slår ihop flera tabeller (list(id, variables, values, codelists)) till det
# som flik 2 visar. Variabler med samma kod blir en lista med värdena från alla
# tabeller, se px_merge_values(). Förvalen är varje tabells förval, så att varje tabell
# ger data från början. Obligatorisk är en variabel bara om alla tabeller kräver den;
# annars hoppas de tabeller som kräver den över när listan är tom.
# Indelningar (kodlistor) med samma namn blir ett val, eftersom id:na kan skilja sig
# mellan tabeller (vs_CKM02Län, vs_RegionLän07). `codelist_groups` ger id:na; en
# indelning som bara vissa tabeller har säger vilka.
# En enda tabell behåller alla sina rader; namn som står flera gånger får koden efter sig.
px_merge_tables <- function(tabs) {
  default_of <- function(t, code) {
    px_default_selection(t$variables$role[match(code, t$variables$code)], t$values[[code]]$code)
  }
  if (length(tabs) == 1) {
    t <- tabs[[1]]
    codes <- t$variables$code
    defaults <- list()
    for (k in codes) {
      vals <- as.data.frame(t$values[[k]])
      key <- px_value_key(vals)
      if (anyDuplicated(key)) t$values[[k]] <- px_disambiguate_labels(vals)
      defaults[[k]] <- default_of(t, k)
    }
    return(list(
      variables = t$variables, values = t$values, codelists = t$codelists,
      defaults = defaults, aliases = list(), codelist_groups = list(), dropped = list(),
      var_tables = stats::setNames(rep(list(t$id), length(codes)), codes)
    ))
  }

  cols <- Reduce(intersect, lapply(tabs, function(t) names(t$variables)))
  all_vars <- do.call(rbind, lapply(tabs, function(t) t$variables[, cols, drop = FALSE]))
  variables <- all_vars[!duplicated(all_vars$code), , drop = FALSE]
  rownames(variables) <- NULL
  values <- list()
  defaults <- list()
  var_tables <- list()
  var_elimination <- list()
  aliases <- list()
  dropped <- list()

  for (i in seq_len(nrow(variables))) {
    code <- variables$code[i]
    rows <- all_vars[all_vars$code == code, , drop = FALSE]
    has <- Filter(function(t) code %in% t$variables$code, tabs)
    variables$label[i] <- paste(unique(rows$label), collapse = " / ")
    variables$elimination[i] <- length(has) < length(tabs) || any(rows$elimination %in% TRUE)
    role <- intersect(c("contents", "time", "geo"), rows$role)
    if (length(role) > 0) variables$role[i] <- role[1]
    if ("show" %in% cols && any(!is.na(rows$show))) variables$show[i] <- rows$show[!is.na(rows$show)][1]

    var_tables[[code]] <- vapply(has, function(t) t$id, "")
    if (length(has) > 1) {
      m <- px_merge_values(stats::setNames(lapply(has, function(t) t$values[[code]]), var_tables[[code]]),
                           variables$role[i])
      vals <- m$values
      if (length(m$alias) > 0) aliases[[code]] <- m$alias
      if (m$dropped > 0) dropped[[code]] <- m$dropped
      defaults[[code]] <- unique(unlist(lapply(has, function(t) {
        unlist(m$to_row[[t$id]][default_of(t, code)], use.names = FALSE)
      }), use.names = FALSE))
    } else {
      vals <- px_disambiguate_labels(as.data.frame(has[[1]]$values[[code]]))
      defaults[[code]] <- default_of(has[[1]], code)
    }
    values[[code]] <- vals
    variables$size[i] <- nrow(vals)
    var_elimination[[code]] <- stats::setNames(
      vapply(has, function(t) isTRUE(t$variables$elimination[match(code, t$variables$code)]), TRUE),
      var_tables[[code]]
    )
  }

  # en rad per indelningsnamn och variabel, med det första id:t som rad-id
  codelist_groups <- list()
  group_rows <- function(k) {
    has <- Filter(function(t) k %in% t$variables$code, tabs)
    rows <- do.call(rbind, lapply(has, function(t) {
      cl <- t$codelists
      if (is.null(cl) || nrow(cl) == 0) return(NULL)
      cl <- as.data.frame(cl)[cl$variable == k, c("variable", "id", "label", "type"), drop = FALSE]
      if (nrow(cl) == 0) return(NULL)
      cl$table <- t$id
      cl
    }))
    if (is.null(rows)) return(NULL)
    rows$key <- tolower(trimws(rows$label))
    out <- rows[!duplicated(rows$key), c("variable", "id", "label", "type"), drop = FALSE]
    for (j in seq_len(nrow(out))) {
      group <- rows[rows$key == tolower(trimws(out$label[j])), , drop = FALSE]
      codelist_groups[[k]][[out$id[j]]] <<- unique(group$id)
      if (length(unique(group$table)) < length(has)) {
        out$label[j] <- paste0(out$label[j], " (bara ",
                               paste(sub("^.*/", "", unique(group$table)), collapse = ", "), ")")
      }
    }
    out
  }
  codelists <- do.call(rbind, lapply(variables$code, group_rows))

  list(variables = variables, values = values, codelists = codelists,
       defaults = defaults, var_tables = var_tables, aliases = aliases, dropped = dropped,
       codelist_groups = codelist_groups, var_elimination = var_elimination)
}

# Värdena i en vald indelning med flera tabeller: märker dem som bara vissa tabeller
# har, som px_merge_tables() gör. per_table: tabell-id -> tabellens koder.
px_mark_common <- function(values, per_table) {
  if (length(per_table) < 2) return(values)
  finns <- lapply(values$code, function(k) names(per_table)[vapply(per_table, function(x) k %in% x, TRUE)])
  values$common <- lengths(finns) == length(per_table)
  bara <- !values$common
  values$label[bara] <- paste0(values$label[bara], " (bara ", vapply(finns[bara], function(x) {
    paste(sub("^.*/", "", x), collapse = ", ")
  }, ""), ")")
  values
}

# Text när en tom lista gör att vissa tabeller inte hämtas, men inte alla.
# elimination: namngiven logisk vektor, tabell-id -> kan hoppas över, för de tabeller
# som har variabeln. n_tables: antal valda tabeller.
px_elimination_note <- function(elimination, n_tables = length(elimination)) {
  if (all(elimination)) return(NULL)
  if (all(!elimination) && length(elimination) == n_tables) return(NULL)
  namn <- function(x) paste(sub("^.*/", "", names(elimination)[x]), collapse = ", ")
  paste0("Obligatorisk i ", namn(!elimination),
         if (any(elimination)) paste0(", kan hoppas över i ", namn(elimination)),
         ". Utan val hämtas inte ", namn(!elimination), ".")
}

# Antal celler per tabell för en sammanslagen fråga, enligt samma regler som
# pxr_pxweb2_get_data() följer: varje tabell får sina egna värden. Har en tabell
# inget av de valda värdena i en variabel den kan utelämna utelämnas variabeln, och
# attributet left_out (tabell-id -> variabelnamn) visar det. Krävs variabeln, eller
# är det en region, hämtas tabellen inte (0 celler), och attributet missing visar
# varför. "9999" är den senaste perioden i varje tabell, och bara i dess tidsvariabel.
# Med en vald indelning gäller dess koder i de tabeller som har den: indelade är
# variabel -> tabell-id -> koder. Värden matchas som i .pxweb2_query_for_table().
px_table_cells <- function(tabs, query, indelade = list()) {
  left_out <- list()
  missing <- list()
  cells <- vapply(tabs, function(t) {
    q <- list()
    borta <- character()
    saknas <- character()
    for (i in seq_len(nrow(t$variables))) {
      code <- t$variables$code[i]
      v <- query[[code]]
      if (is.null(v) || (length(v) == 1 && is.na(v)) || identical(v, "*")) {
        if (!identical(v, "*") && !isTRUE(t$variables$elimination[i])) {
          saknas <- c(saknas, t$variables$label[i])
        }
        q[[code]] <- v
        next
      }
      koder <- indelade[[code]][[t$id]]
      ok <- if (is.null(koder)) t$values[[code]] else list(code = koder)
      v <- v[.pxweb2_match_values(v, ok) | (v == "9999" & identical(t$variables$role[i], "time"))]
      if (length(v) == 0) {
        if (!isTRUE(t$variables$elimination[i]) || identical(t$variables$role[i], "geo")) {
          saknas <- c(saknas, t$variables$label[i])
        } else {
          borta <- c(borta, t$variables$label[i])
        }
        v <- NA
      }
      q[[code]] <- v
    }
    if (length(saknas) > 0) {
      missing[[t$id]] <<- saknas
      return(0)
    }
    if (length(borta) > 0) left_out[[t$id]] <<- borta
    px_cell_count(t$variables, q)
  }, 0)
  if (length(left_out) > 0) attr(cells, "left_out") <- left_out
  if (length(missing) > 0) attr(cells, "missing") <- missing
  cells
}

# Rader om vad som händer med tabeller som saknar valda värden, för koden
px_table_notes <- function(left_out = NULL, missing = NULL) {
  kort <- function(id) sub("^.*/", "", id)
  c(vapply(names(left_out), function(id) {
    paste0(kort(id), " hämtas utan ", paste(left_out[[id]], collapse = ", "),
           ": saknar de valda värdena, så raderna gäller alla värden")
  }, "", USE.NAMES = FALSE),
  vapply(names(missing), function(id) {
    paste0(kort(id), " hämtas inte: inga giltiga värden valda för ", paste(missing[[id]], collapse = ", "))
  }, "", USE.NAMES = FALSE))
}

# Filnamn: tabellens id, eller första id:t när det är flera tabeller
px_file_id <- function(id) {
  if (length(id) > 1) paste0(id[1], "_m_fl") else id
}

px_r_value <- function(v) {
  if (length(v) == 1 && is.na(v)) return("NA")
  v <- gsub("\\", "\\\\", v, fixed = TRUE)   # \ -> \\ först, sedan "
  q <- paste0('"', gsub('"', '\\"', v, fixed = TRUE), '"')
  if (length(q) == 1) q else paste0("c(", paste(q, collapse = ", "), ")")
}

# R-kod som återskapar hämtningen. Variabelnamn som inte är giltiga
# R-namn omges av backticks. Språk (lang) och webbplats (base_url) skrivs
# alltid ut, så att koden ger samma resultat oavsett inställningar.
px_query_code <- function(table_id, title, query, include_aggregations = NULL,
                          object_name = "dataset_df", code_variables = NULL,
                          lang = "sv",
                          base_url = "https://statistikdatabasen.scb.se/api/v2/tables/",
                          provider_name = NULL, clean_names = FALSE, provider = NULL,
                          notes = NULL) {
  r_name <- function(n) if (make.names(n) == n) n else paste0("`", n, "`")
  names_r <- vapply(names(query), r_name, "")
  vals <- vapply(query, px_r_value, "")
  lines <- paste0("    ", names_r, " = ", vals)
  agg <- ""
  if (length(include_aggregations) > 0) {
    agg <- paste0(
      ",\n  include_aggregations = c(",
      paste0(vapply(names(include_aggregations), r_name, ""), " = ",
             vapply(include_aggregations, px_r_value, ""), collapse = ", "),
      ")"
    )
  }
  kod <- ""
  if (length(code_variables) > 0) {
    kod <- paste0(",\n  code_variables = ", px_r_value(unname(code_variables)))
  }
  site <- paste0(",\n  lang = ", px_r_value(lang), ",\n  base_url = ", px_r_value(base_url))
  # namnet att ange som provider i pxr_-funktionerna står efter leverantören
  if (length(provider_name) == 1 && nzchar(provider_name) && length(provider) == 1) {
    provider_name <- paste0(provider_name, " (provider = ", px_r_value(provider), ")")
  }
  paste0(
    "library(pxweb2rgui)\n\n",
    if (length(provider_name) == 1 && nzchar(provider_name)) paste0("# ", provider_name, "\n"),
    paste0("# ", title, "\n", collapse = ""),
    if (length(notes) > 0) paste0("# ", notes, "\n", collapse = ""),
    object_name, " <- pxr_pxweb2_get_data(\n",
    "  table = ", px_r_value(table_id), ",\n",
    "  query = list(\n",
    paste(lines, collapse = ",\n"),
    "\n  )", agg, kod, site, "\n)\n",
    if (isTRUE(clean_names)) {
      paste0("\n# R-kompatibla kolumnnamn\n", object_name, " <- janitor::clean_names(",
             object_name, ")\n")
    }
  )
}

# Kolumnnamnen som janitor::clean_names() ger. Utan janitor returneras
# data oförändrat.
# Hämtat data har kodkolumner för alla variabler; de som inte är valda tas bort.
# Kolumnen heter <variabel>_code, och namnet på variabeln kan skilja sig mellan tabeller.
px_keep_code_columns <- function(d, tables, code_variables) {
  dolda <- unlist(lapply(tables, function(t) {
    paste0(t$variables$label[!t$variables$code %in% code_variables], "_code")
  }))
  d[, !names(d) %in% dolda, drop = FALSE]
}

px_clean_names <- function(d) {
  if (!requireNamespace("janitor", quietly = TRUE)) return(d)
  janitor::clean_names(d)
}

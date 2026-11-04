# Fliken "4. Kommentarer": kommentarerna till de valda tabellerna, en ruta per tabell
# så att det syns vilken tabell varje kommentar hör till.

# Kommentarerna till en tabell, som list(rows, error, warning). Det som hämtats sparas
# per webbplats, språk och tabell, eftersom det tar några sekunder med PxWeb API v1.
# Ett fel gäller bara den tabellen, och sparas inte.
px_table_comments <- function(backend, id, site, cache) {
  key <- paste(site$base_url, site$lang, id)
  if (!is.null(cache[[key]])) return(cache[[key]])
  varning <- NULL
  res <- tryCatch(
    suppressMessages(withCallingHandlers(
      list(rows = as.data.frame(backend$get_comments(site$base_url, tab = id, lang = site$lang))),
      warning = function(w) {
        varning <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )),
    error = function(e) list(error = conditionMessage(e))
  )
  res$warning <- varning
  if (is.null(res$error)) cache[[key]] <- res
  res
}

# Texten som HTML. All text escapas, och bara länkar till http(s) i leverantörernas
# två skrivsätt, <a href='...'>namn</a> och [namn](...), blir klickbara.
px_comment_html <- function(text) {
  x <- htmltools::htmlEscape(enc2utf8(text))
  lank <- function(url, namn) {
    paste0('<a href="', url, '" target="_blank" rel="noopener noreferrer">', namn, "</a>")
  }
  x <- gsub("(?i)&lt;a\\s+href\\s*=\\s*['\"](https?://[^'\"\\s]+)['\"].*?&gt;(.*?)&lt;/a&gt;",
            lank("\\1", "\\2"), x, perl = TRUE)
  x <- gsub("\\[([^\\]\\n]+)\\]\\((https?://[^)\\s'\"]+)\\)", lank("\\2", "\\1"), x, perl = TRUE)
  htmltools::HTML(x)
}

px_comment_item <- function(row) {
  div(
    class = "px-kommentar",
    if (!is.na(row$varde_kod)) {
      div(class = "px-kommentar-varde", row$varde_namn %|na|% row$varde_kod,
          tags$code(class = "ms-1", row$varde_kod))
    },
    # .noWS: radbrytningar i texten visas, och då får html:en inte ha några egna
    tags$span(class = "px-kommentar-text", .noWS = "inside", px_comment_html(row$kommentar))
  )
}

`%|na|%` <- function(a, b) if (is.na(a) || !nzchar(a)) b else a

# Kommentarerna i en tabell: först de på hela tabellen, sedan per variabel
px_comment_rows_ui <- function(rows) {
  items <- function(d) lapply(seq_len(nrow(d)), function(i) px_comment_item(d[i, , drop = FALSE]))
  hela <- rows[is.na(rows$var_kod), , drop = FALSE]
  tagList(
    if (nrow(hela) > 0) tagList(tags$h6(class = "fw-semibold", "Hela tabellen"), items(hela)),
    lapply(unique(rows$var_kod[!is.na(rows$var_kod)]), function(kod) {
      d <- rows[rows$var_kod %in% kod, , drop = FALSE]
      tagList(tags$h6(class = "fw-semibold mt-3", paste0("Variabel: ", d$var_namn[1] %|na|% kod)), items(d))
    })
  )
}

# En ruta per tabell, med tabell-id och tabellnamn som rubrik. Rutorna får vara så höga
# som texten: annars krymper de och kommentarerna göms i en egen rullningslist.
px_comment_card <- function(id, title, res) {
  antal <- if (is.null(res$error)) nrow(res$rows) else 0L
  card(
    class = "px-kommentar-tabell mb-3", fill = FALSE,
    card_header(tags$code(class = "me-2", id), title,
                if (antal > 0) tags$span(class = "px-count", antal)),
    card_body(
      fill = FALSE, fillable = FALSE,
      if (!is.null(res$error)) {
        div(class = "text-danger", paste0("Kunde inte hämta kommentarerna till ", id, ": ", res$error))
      } else if (antal == 0) {
        p(class = "text-muted mb-0", paste0("Inga kommentarer för ", id, "."))
      } else {
        px_comment_rows_ui(res$rows)
      },
      if (!is.null(res$warning)) div(class = "text-warning small mt-2", res$warning)
    )
  )
}

# Gränssnittet: px_app_ui() och px_app_server() används både av
# pxweb2_gui() (fristående eller som RStudio-addin) och av testerna.
#
# 0. Välj statistikleverantör (webbplats med PxWeb API v1 eller v2) och språk
# 1. Bläddra i leverantörens ämnesträd (ämnesområden, underområden och tabeller)
# 2. Välj tabell och variabler/värden
# 3. Få färdig pxr_pxweb2_get_data()-kod och/eller hämta data direkt
# 4. Läs kommentarerna till de valda tabellerna, efter klick på knappen i flik 1

px_tree_js <- "
// Valda tabeller i den ordning de valdes. Ett klick väljer en tabell; Ctrl+klick
// (Cmd på Mac) lägger till eller tar bort en. Urvalet finns kvar när en sökning
// ritar om trädet.
var pxValda = [];
function pxMarkTables() {
  $('.px-tree a.px-selected').removeClass('px-selected');
  $('.px-tree a.px-table').filter(function() {
    return pxValda.indexOf(this.getAttribute('data-id')) >= 0;
  }).addClass('px-selected');
}
$(document).on('click', '.px-tree a.px-table', function(e) {
  e.preventDefault();
  // attr, inte data: jQuery gör om ett id som 10531 till ett tal
  var id = this.getAttribute('data-id');
  if (e.ctrlKey || e.metaKey) {
    var i = pxValda.indexOf(id);
    if (i >= 0) pxValda.splice(i, 1); else pxValda.push(id);
  } else {
    pxValda = [id];
  }
  pxMarkTables();
  // en tom lista blir NULL i Shiny, som då inte ser händelsen; '' betyder inget valt
  Shiny.setInputValue('vald_tabell', pxValda.length ? pxValda.slice() : '', {priority: 'event'});
});
Shiny.addCustomMessageHandler('pxSetTables', function(ids) {
  pxValda = ids;
  pxMarkTables();
});
// Fliken Kommentarer är gråtonad och går inte att klicka på när den saknar innehåll
function pxKommentarFlik(aktiv) {
  $('a[data-value=\"kommentarer\"]').toggleClass('disabled', !aktiv)
    .attr('aria-disabled', aktiv ? null : 'true');
}
$(function() { pxKommentarFlik(false); });
Shiny.addCustomMessageHandler('pxKommentarFlik', pxKommentarFlik);
$(document).on('click', '#expandera_alla', function() {
  $('.px-tree details').prop('open', true);
});
$(document).on('click', '#fall_ihop_alla', function() {
  $('.px-tree details').prop('open', false);
});
// Kopiering: urklipps-API:t fungerar inte i alla fönster (t.ex. RStudios
// Viewer), så vid behov provas execCommand och sist servern (clipr).
function pxCopied(ok) {
  $('#kopiera_kod').text(ok ? 'Kopierad!' : 'Kopiera koden');
  if (ok) setTimeout(function() { $('#kopiera_kod').text('Kopiera koden'); }, 1500);
}
function pxCopyFallback(txt) {
  var ta = $('<textarea readonly>').val(txt)
    .css({position: 'fixed', top: 0, left: 0, opacity: 0}).appendTo('body');
  ta[0].focus(); ta[0].select();
  var ok = false;
  try { ok = document.execCommand('copy'); } catch (e) { ok = false; }
  ta.remove();
  if (ok) pxCopied(true);
  else Shiny.setInputValue('kopiera_via_server', Math.random());
}
$(document).on('click', '#kopiera_kod', function() {
  var txt = $('#kod').text();
  if (navigator.clipboard && window.isSecureContext) {
    navigator.clipboard.writeText(txt).then(function() { pxCopied(true); },
                                            function() { pxCopyFallback(txt); });
  } else {
    pxCopyFallback(txt);
  }
});
Shiny.addCustomMessageHandler('pxCopied', function(ok) { pxCopied(ok); });
// efter Stäng/Infoga: visa att appen är stängd (t.ex. i Positrons Viewer,
// där sidan annars ligger kvar) och stäng fliken om webbläsaren tillåter det
Shiny.addCustomMessageHandler('pxClose', function(x) {
  var d = document.createElement('div');
  d.style.cssText = 'font-family: system-ui, sans-serif; padding: 3rem; text-align: center; color: #555;';
  var h = document.createElement('h3');
  h.textContent = 'pxweb2rgui är stängt';
  var p = document.createElement('p');
  p.textContent = (x && x.inserted ? 'Koden har infogats i skriptet. ' : '') +
    'Du kan stänga den här fliken eller panelen.';
  d.appendChild(h); d.appendChild(p);
  document.body.innerHTML = '';
  document.body.appendChild(d);
  setTimeout(function() { try { window.close(); } catch (e) {} }, 150);
});
"

px_tree_css <- "
.px-tree { font-size: 0.95rem; }
.px-tree details { margin-left: 0.2rem; }
.px-tree summary { cursor: pointer; padding: 2px 0; }
.px-tree summary:hover .px-folder { text-decoration: underline; }
.px-children { margin-left: 1.1rem; border-left: 1px solid var(--bs-border-color); padding-left: 0.6rem; }
.px-count { margin-left: 0.4rem; font-size: 0.8em; color: var(--bs-secondary-color);
            background: var(--bs-tertiary-bg); border-radius: 0.6em; padding: 0 0.45em; }
.px-tables { list-style: none; padding-left: 0.2rem; margin: 0.15rem 0 0.4rem 0; }
.px-tables li { padding: 1px 0; }
.px-table { text-decoration: none; }
.px-table:hover .px-table-label { text-decoration: underline; }
.px-table-id { font-family: var(--bs-font-monospace); font-size: 0.85em; margin-right: 0.4rem;
               color: var(--bs-secondary-color); }
.px-selected { background: var(--bs-warning-bg-subtle); border-radius: 3px; }
.px-discontinued .px-table-label { color: var(--bs-secondary-color); }
.px-period { margin-left: 0.4rem; font-size: 0.8em; color: var(--bs-secondary-color); }
.px-badge { margin-left: 0.4rem; font-size: 0.75em; color: var(--bs-danger); }
.px-var-card .card-header { font-weight: 600; }
#kod { font-size: 0.85rem; user-select: all; }
.px-kommentarer-sida { flex: 1 1 auto; min-height: 0; overflow-y: auto; }
.px-kommentar-tabell { flex: none; overflow: visible; }
.px-kommentar { padding: 0.35rem 0; border-top: 1px solid var(--bs-border-color); }
.px-kommentar-varde { font-weight: 600; font-size: 0.9rem; }
.px-kommentar-text { white-space: pre-line; }
"

# addin = TRUE lägger till knapparna "Infoga koden i skriptet" och "Stäng",
# som avslutar gadgeten och återgår till RStudio.
px_app_ui <- function(addin = FALSE) {
  page_navbar(
    title = "pxweb2rgui",
    id = "flikar",
    theme = bs_theme(version = 5),
    header = tags$head(tags$script(HTML(px_tree_js)), tags$script(HTML(px_var_js)),
                       tags$style(HTML(px_tree_css)), tags$style(HTML(px_var_css))),

    nav_panel(
      "1. Hitta tabell", value = "tabell",
      layout_sidebar(
        sidebar = sidebar(
          width = 340,
          selectInput("leverantor", "Statistikleverantör", choices = px_provider_choices(),
                      selected = px_start_provider(), selectize = FALSE),
          conditionalPanel(
            "input.leverantor == '_annan'",
            textInput("annan_url", "Adress till PxWeb API v1 eller v2",
                      placeholder = "https://.../api/v2/ eller https://.../api/v1/sv/databas/")
          ),
          uiOutput("provider_namn"),
          selectInput("sprak", "Språk", choices = NULL, selectize = FALSE),
          uiOutput("api_v1_info"),
          hr(),
          textInput("sok", "Sök i tabellnamn eller tabell-id",
                    placeholder = "t.ex. folkmängd kommun"),
          checkboxInput("avslutade", "Visa även avslutade tabeller", FALSE),
          div(
            class = "d-flex gap-2",
            actionButton("expandera_alla", "Expandera alla", class = "btn-sm"),
            actionButton("fall_ihop_alla", "Fäll ihop alla", class = "btn-sm")
          ),
          hr(),
          uiOutput("vald_tabell_info"),
          hr(),
          uiOutput("katalog_status"),
          actionButton("uppdatera", "Hämta tabellistan på nytt",
                       class = "btn-sm btn-outline-secondary")
        ),
        card(
          full_screen = TRUE,
          # i rubriken syns knappen alltid; sidopanelen behöver ofta rullas
          card_header(class = "d-flex justify-content-between align-items-center",
                      "Ämnesområden, underområden och tabeller",
                      uiOutput("kommentarer_knapp")),
          uiOutput("trad")
        )
      )
    ),

    nav_panel(
      "2. Välj variabler", value = "variabler",
      uiOutput("variabel_rubrik"),
      uiOutput("variabler")
    ),

    nav_panel(
      "3. Kod och data", value = "data",
      layout_columns(
        col_widths = c(5, 7),
        card(
          card_header("R-kod"),
          uiOutput("fraga_status"),
          div(actionButton("valj_alla", "Välj alla värden",
                           class = "btn-sm btn-outline-secondary")),
          uiOutput("kod_variabler_ui"),
          checkboxInput("clean_names",
                        tagList("R-kompatibla kolumnnamn med janitor::clean_names()",
                                tags$small(class = "text-muted fw-normal",
                                           " (t.ex. region, kon, folkmangd)")),
                        value = getOption("pxweb2rgui.clean_names", FALSE)),
          uiOutput("clean_names_info"),
          verbatimTextOutput("kod"),
          div(
            class = "d-flex gap-2",
            tags$button(id = "kopiera_kod", type = "button",
                        class = "btn btn-sm btn-outline-primary", "Kopiera koden"),
            downloadButton("ladda_ner_skript", "Ladda ner som .R", class = "btn-sm")
          ),
          if (addin) {
            actionButton("infoga_kod", "Infoga koden i skriptet och stäng",
                         class = "btn-primary mt-3")
          }
        ),
        card(
          full_screen = TRUE,
          card_header("Data"),
          div(
            class = "d-flex gap-2 mb-2",
            actionButton("hamta_data", "Hämta data", class = "btn-primary"),
            div(class = "ms-auto d-flex gap-2 align-items-center",
                selectInput("format", NULL, choices = px_export_formats, width = "230px",
                            selectize = FALSE),
                downloadButton("ladda_ner_data", "Ladda ner", class = "btn-sm"))
          ),
          uiOutput("format_info"),
          uiOutput("data_status"),
          uiOutput("data_tabell_ruta")
        )
      )
    ),

    # gråtonad tills knappen "Kommentarer" i flik 1 har gett den innehåll
    nav_panel(
      "4. Kommentarer", value = "kommentarer",
      div(class = "px-kommentarer-sida", uiOutput("kommentarer"))
    ),

    nav_spacer(),
    if (addin) {
      nav_item(actionButton("stang", "Stäng", class = "btn-sm btn-outline-secondary"))
    }
  )
}

px_app_server <- function(addin = FALSE) {
  function(input, output, session) {
    backend <- px_backend()

    # --- Avslut ------------------------------------------------------------------
    # Appen stoppas bara en gång. Stängs fönstret eller fliken (t.ex. i
    # Positron eller webbläsaren) avslutas den också, så att R inte blir
    # hängande i en gadget utan fönster.
    # Vid Stäng/Infoga skickas först meddelandet till sidan, och appen
    # stoppas först när det har gått iväg (onFlushed).
    stoppad <- FALSE
    px_stop <- function(value = NULL, inserted = FALSE, now = FALSE) {
      if (stoppad) return(invisible(NULL))
      stoppad <<- TRUE
      if (now) return(try(px_stop_app(value), silent = TRUE))
      try(session$sendCustomMessage("pxClose", list(inserted = inserted)), silent = TRUE)
      session$onFlushed(function() try(px_stop_app(value), silent = TRUE), once = TRUE)
    }
    session$onSessionEnded(function() px_stop(invisible(NULL), now = TRUE))

    # --- Addin: infoga koden i RStudio och stäng -------------------------------
    if (addin) {
      observeEvent(input$stang, px_stop(invisible(NULL)))
      observeEvent(input$infoga_kod, {
        if (is.null(tabell())) {
          showNotification("Välj först en tabell.", type = "warning")
          return()
        }
        code <- fraga()$code
        px_insert_code(code)
        px_stop(invisible(code), inserted = TRUE)
      })
    }

    # --- Statistikleverantör och språk -----------------------------------------
    leverantorer <- px_providers_df(backend)
    annan_url <- debounce(reactive(input$annan_url), 800)

    webbplats <- reactive({
      id <- input$leverantor %||% px_start_provider()
      if (identical(id, "_annan")) {
        url <- trimws(annan_url() %||% "")
        req(grepl("^https?://[^/]+", url))
        # en adress till API v1 är inte klar förrän språket är inskrivet
        req(!.pxweb1_is_url(url) || !is.null(.pxweb1_parts(url)))
        base_url <- .pxweb1_latest_url(px_tables_url(url))
        return(list(base_url = base_url, provider = base_url,
                    name = sub("^https?://([^/]+).*$", "\\1", url)))
      }
      i <- match(id, leverantorer$id)
      if (is.na(i)) i <- 1L
      # har leverantören också API v2 används alltid det
      list(base_url = .pxweb1_latest_url(leverantorer$tables_url[i]),
           provider = leverantorer$id[i], name = leverantorer$name[i])
    })

    # namnet som anges som provider i pxr_-funktionerna; för en egen webbplats
    # är det adressen
    output$provider_namn <- renderUI({
      id <- input$leverantor %||% px_start_provider()
      namn <- if (identical(id, "_annan")) webbplats()$base_url else id
      div(class = "border rounded p-2 mb-3 small",
          div(class = "text-muted", "Namn att ange som provider"),
          tags$code(class = "px-provider-namn", style = "word-break: break-all;", namn))
    })

    # det som inte finns i API v1 ska användaren få veta när webbplatsen väljs
    output$api_v1_info <- renderUI({
      if (.dst_is_url(webbplats()$base_url)) {
        return(div(class = "small text-muted mt-2",
                   tags$strong("Danmarks Statistik. "),
                   "Eget API, inte PxWeb. \"Välj indelning\" (kodlistor och aggregeringar) ",
                   "finns inte."))
      }
      if (!.pxweb1_is_url(webbplats()$base_url)) return(NULL)
      div(class = "small text-muted mt-2",
          tags$strong("PxWeb API v1. "),
          "Tabell-id är tabellens sökväg. \"Välj indelning\" (kodlistor och aggregeringar) ",
          "finns inte, avslutade tabeller går inte att skilja ut och perioder visas inte ",
          "i tabellistan. Tabellistan kan ta några minuter att hämta första gången.")
    })

    sprak_koder <- reactiveVal(character())
    observeEvent(webbplats(), {
      langs <- tryCatch(as.data.frame(backend$languages(webbplats()$base_url)),
                        error = function(e) px_fallback_languages(webbplats()$base_url))
      choices <- px_language_choices(langs)
      cur <- isolate(input$sprak)
      sel <- if (!is.null(cur) && cur %in% choices) cur else {
        d <- attr(langs, "default") %||% getOption("pxweb2r.lang")
        if (!is.null(d) && d %in% choices) d else unname(choices[1])
      }
      sprak_koder(unname(choices))
      updateSelectInput(session, "sprak", choices = choices, selected = sel)
    })

    # webbplats och språk som gäller; väntar tills språklistan hör till
    # den valda webbplatsen
    site <- reactive({
      w <- webbplats()
      req(input$sprak, input$sprak %in% sprak_koder())
      list(base_url = w$base_url, lang = input$sprak, name = w$name, provider = w$provider)
    })

    max_celler <- reactive({
      tryCatch(as.numeric(backend$max_cells(site()$base_url)), error = function(e) 150000)
    })

    # --- Tabellkatalog ---------------------------------------------------------
    katalog <- reactiveVal(NULL)
    katalog_fel <- reactiveVal(NULL)

    lasa_katalog <- function(force = FALSE) {
      s <- isolate(site())
      withProgress(message = paste0("Hämtar tabellistan från ", s$name, "…"), value = 0.3, {
        res <- tryCatch(px_fetch_catalogue(backend, force = force,
                                           base_url = s$base_url, lang = s$lang),
                        error = function(e) e)
      })
      if (inherits(res, "error")) {
        katalog(NULL)
        katalog_fel(conditionMessage(res))
      } else {
        katalog_fel(NULL)
        katalog(res)
      }
    }

    # ny webbplats eller nytt språk: ny tabellista, och vald tabell nollställs
    observeEvent(site(), {
      tabell(NULL)
      valda(character())
      session$sendCustomMessage("pxSetTables", list())
      indelning(list())
      rensa_data()
      katalog(NULL)
      lasa_katalog()
    })
    observeEvent(input$uppdatera, lasa_katalog(force = TRUE))

    sok <- debounce(reactive(input$sok), 400)

    filtrerade <- reactive({
      req(katalog())
      px_filter_tables(katalog()$tables, sok(), isTRUE(input$avslutade))
    })

    trad <- reactive({
      req(katalog())
      px_build_tree(katalog()$paths, filtrerade()$id)
    })

    output$trad <- renderUI({
      if (!is.null(katalog_fel())) {
        return(div(class = "text-danger",
                   "Kunde inte hämta tabellistan: ", katalog_fel()))
      }
      if (is.null(katalog())) return(p("Hämtar tabellistan…"))
      t <- trad()
      if (t$n == 0) return(p("Inga tabeller matchar sökningen."))
      # vid sökning fälls hela det (filtrerade) trädet ut
      px_tree_html(t, katalog()$tables, open = nzchar(trimws(sok() %||% "")),
                   selected = isolate(valda()))
    })

    output$katalog_status <- renderUI({
      k <- katalog()
      if (is.null(k)) return(NULL)
      s <- px_tree_stats(trad())
      tags$small(
        class = "text-muted",
        sprintf("%d ämnesområden, %d mappar och %d tabeller visas. ",
                s$subjects, s$folders, s$tables),
        sprintf("Tabellistan hämtades %s.", format(k$fetched, "%Y-%m-%d %H:%M"))
      )
    })

    # --- Vald tabell -----------------------------------------------------------
    tabell <- reactiveVal(NULL)
    valda <- reactiveVal(character())
    laddningar <- reactiveVal(0)

    las_tabell <- function(id, s) {
      meta <- backend$get_metadata(id, base_url = s$base_url, lang = s$lang)
      list(
        id = id,
        title = meta$label %||% id,
        meta = meta,
        variables = as.data.frame(backend$get_variables(meta)),
        values = backend$get_values(meta, include_aggregations = "none",
                                    return_df_if_only_one_variable = FALSE,
                                    quiet = TRUE),
        # andra indelningar (t.ex. län/kommuner) som tabellen erbjuder
        codelists = tryCatch(as.data.frame(backend$list_codelists(meta, lang = s$lang)),
                             error = function(e) NULL)
      )
    }

    # en eller flera tabeller; med flera är variabler och värden sammanslagna
    observeEvent(input$vald_tabell, {
      ids <- as.character(input$vald_tabell)
      ids <- ids[nzchar(ids)]
      if (length(ids) == 0) {
        valda(ids)
        indelning(list())
        rensa_data()
        tabell(NULL)
        return()
      }
      res <- withProgress(message = paste("Hämtar metadata för", paste(ids, collapse = ", ")),
                          value = 0.5, {
        tryCatch({
          s <- site()
          tabs <- lapply(ids, las_tabell, s = s)
          c(list(id = ids, site = s,
                 title = vapply(tabs, function(t) t$title, ""),
                 meta = tabs[[1]]$meta, tables = tabs),
            px_merge_tables(tabs))
        }, error = function(e) e)
      })
      if (inherits(res, "error")) {
        showNotification(paste("Kunde inte hämta tabellen:", conditionMessage(res)),
                         type = "error", duration = NULL)
        # allt är som före klicket, även markeringen i trädet
        session$sendCustomMessage("pxSetTables", as.list(valda()))
        return()
      }
      valda(ids)
      indelning(list())
      rensa_data()
      laddningar(laddningar() + 1)
      res$prefix <- paste0("t", laddningar(), "_")
      tabell(res)
    })

    output$vald_tabell_info <- renderUI({
      t <- tabell()
      if (is.null(t)) {
        return(tags$small(class = "text-muted",
                          "Klicka på en tabell i trädet för att välja den.",
                          "Ctrl+klick väljer flera."))
      }
      tagList(
        if (length(t$id) > 1) tags$strong(paste(length(t$id), "tabeller valda")),
        lapply(seq_along(t$id), function(i) {
          div(class = "mb-1", tags$strong(t$id[i]), br(), t$title[i])
        }),
        tags$small(class = "text-muted",
                   paste(nrow(t$variables), "variabler:",
                         paste(t$variables$label, collapse = ", "))),
        div(class = "mt-2",
            actionButton("till_variabler", "Välj variabler →",
                         class = "btn-primary btn-sm"))
      )
    })

    observeEvent(input$till_variabler, nav_select("flikar", "variabler"))

    # --- Variabelval -----------------------------------------------------------
    var_id <- function(t, kind, i) paste0(t$prefix, kind, "_", i)

    output$variabel_rubrik <- renderUI({
      t <- tabell()
      if (is.null(t)) return(p("Välj först en tabell under flik 1."))
      tagList(
        h4("Välj variabler"),
        lapply(seq_along(t$id), function(i) p(class = "mb-1", t$title[i], " ", tags$code(t$id[i])))
      )
    })

    observeEvent(input$till_data, nav_select("flikar", "data"))

    # --- Kommentarer -----------------------------------------------------------
    kommentar_cache <- new.env(parent = emptyenv())
    # det som visas i fliken: list(id, title, res), eller NULL när den är tom
    kommentar_innehall <- reactiveVal(NULL)

    output$kommentarer_knapp <- renderUI({
      knapp <- actionButton("hamta_kommentarer", "Kommentarer",
                            class = "btn-sm btn-outline-secondary")
      if (length(valda()) == 0) tagAppendAttributes(knapp, disabled = NA) else knapp
    })

    observeEvent(input$hamta_kommentarer, {
      t <- tabell()
      req(t)
      res <- withProgress(message = "Hämtar kommentarer…", value = 0, {
        lapply(t$id, function(id) {
          r <- px_table_comments(backend, id, t$site, kommentar_cache)
          incProgress(1 / length(t$id))
          r
        })
      })
      kommentar_innehall(list(id = t$id, title = t$title, res = res))
      nav_select("flikar", "kommentarer")
    })

    # kommentarerna gäller ett visst val: är det ändrat eller tomt raderas de
    observeEvent(valda(), kommentar_innehall(NULL), ignoreInit = TRUE)

    observe(px_kommentar_flik(session, !is.null(kommentar_innehall())))

    # inget sparas till nästa körning; kommentar_innehall upphör med sessionen
    session$onSessionEnded(function() {
      rm(list = ls(kommentar_cache, all.names = TRUE), envir = kommentar_cache)
    })

    output$kommentarer <- renderUI({
      k <- kommentar_innehall()
      req(k)
      tagList(
        h4(if (length(k$id) > 1) "Kommentarer till valda tabeller" else "Kommentarer till vald tabell"),
        lapply(seq_along(k$id), function(i) px_comment_card(k$id[i], k$title[i], k$res[[i]]))
      )
    })

    # en rensning ska nå sidan direkt, även när fliken inte visas
    outputOptions(output, "kommentarer", suspendWhenHidden = FALSE)

    output$variabler <- renderUI({
      t <- tabell()
      req(t)
      v <- t$variables
      flera <- length(t$id) > 1
      i_alla <- lengths(t$var_tables[v$code]) == length(t$id)
      box <- function(i) {
        cl <- t$codelists
        cl <- if (!is.null(cl) && nrow(cl) > 0) cl[cl$variable == v$code[i], , drop = FALSE]
        px_var_box(var_id(t, "varden", i), v$label[i], t$values[[v$code[i]]],
                   v$role[i], v$elimination[i], show = v$show[i] %||% NA,
                   codelists = cl, selected = t$defaults[[v$code[i]]],
                   common_toggle = flera,
                   note = if (flera) {
                     c(if (!i_alla[i]) {
                       paste("Finns i:", paste(sub("^.*/", "", t$var_tables[[v$code[i]]]), collapse = ", "))
                     }, px_elimination_note(t$var_elimination[[v$code[i]]], length(t$id)),
                     if (!is.null(t$dropped[[v$code[i]]])) {
                       paste0(t$dropped[[v$code[i]]], " värden har samma namn som ett annat värde ",
                              "i samma tabell och kan inte väljas här.")
                     })
                   })
      }
      tagList(
        if (flera) {
          tagList(
            if (any(i_alla)) {
              tagList(h5("Variabler som alla tabeller har"),
                      p(class = "small text-muted",
                        "Listorna har värdena från alla tabeller. Varje tabell hämtas med de valda värden den har."),
                      div(class = "px-vars mb-4", lapply(which(i_alla), box)))
            },
            if (any(!i_alla)) {
              tagList(h5("Variabler som bara finns i vissa tabeller"),
                      div(class = "px-vars", lapply(which(!i_alla), box)))
            }
          )
        } else {
          div(class = "px-vars", lapply(seq_len(nrow(v)), box))
        },
        div(
          class = "mt-4",
          actionButton("till_data", "Fortsätt", class = "btn-primary px-3"),
          uiOutput("cell_info", class = "mt-2")
        )
      )
    })

    # listorna behövs även om flik 2 inte har öppnats, för "Välj alla värden"
    outputOptions(output, "variabler", suspendWhenHidden = FALSE)

    # Listorna i flik 2 markeras, så att flikarna och koden stämmer överens.
    # px_build_query() gör sedan om "alla värden" till "*".
    observeEvent(input$valj_alla, {
      t <- tabell()
      if (is.null(t)) {
        showNotification("Välj först en tabell.", type = "warning")
        return()
      }
      session$sendCustomMessage("pxMarkAll", as.list(vapply(
        px_mark_all_positions(t$variables), function(i) var_id(t, "varden", i), "")))
    })

    # Vald indelning per variabel: variabelkod -> list(id, type, values)
    indelning <- reactiveVal(list())
    kodlistor_cache <- new.env(parent = emptyenv())

    observeEvent(input$px_indelning, {
      t <- tabell()
      req(t)
      ev <- input$px_indelning
      i <- match(ev$target, vapply(seq_len(nrow(t$variables)),
                                   function(k) var_id(t, "varden", k), ""))
      req(!is.na(i))
      var <- t$variables$code[i]
      cur <- indelning()

      if (!nzchar(ev$codelist %||% "")) {
        cur[[var]] <- NULL
        values <- t$values[[var]]
      } else {
        cl_id <- ev$codelist
        hamta <- function(id) {
          cache_key <- paste(t$site$base_url, t$site$lang, id)
          vals <- kodlistor_cache[[cache_key]]
          if (is.null(vals)) {
            vals <- as.data.frame(backend$get_codelist(
              id, base_url = t$site$base_url, lang = t$site$lang))
            kodlistor_cache[[cache_key]] <- vals
          }
          vals
        }
        # med flera tabeller kan samma indelning ha olika id per tabell
        ids <- t$codelist_groups[[var]][[cl_id]] %||% cl_id
        lists <- withProgress(message = "Hämtar indelningen…", value = 0.5,
                              tryCatch(lapply(ids, hamta), error = function(e) e))
        if (inherits(lists, "error")) {
          showNotification(paste("Kunde inte hämta indelningen:", conditionMessage(lists)),
                           type = "error")
          return()
        }
        values <- lists[[1]]
        if (length(ids) > 1) {
          values <- do.call(rbind, lapply(lists, function(d) d[, c("code", "label"), drop = FALSE]))
          values <- values[!duplicated(values$code), , drop = FALSE]
        }
        type <- t$codelists$type[match(cl_id, t$codelists$id)]
        # varje tabells koder, och de aggregeringar som koden måste ange
        koder <- list()
        agg <- character()
        for (k in seq_along(t$tables)) {
          tcl <- t$tables[[k]]$codelists
          hit <- if (!is.null(tcl) && nrow(tcl) > 0) match(ids, tcl$id) else NA
          j <- which(!is.na(hit))
          if (length(j) > 0) {
            koder[[t$tables[[k]]$id]] <- lists[[j[1]]]$code
            if (identical(tolower(tcl$type[hit[j[1]]]), "aggregation")) agg <- c(agg, ids[j[1]])
          }
        }
        # tabeller utan indelningen har sina egna koder
        har_var <- Filter(function(x) var %in% x$variables$code, t$tables)
        per_tabell <- lapply(har_var, function(x) koder[[x$id]] %||% x$values[[var]]$code)
        names(per_tabell) <- vapply(har_var, function(x) x$id, "")
        values <- px_mark_common(values, per_tabell)
        cur[[var]] <- list(id = cl_id, type = type, values = values, koder = koder,
                           agg_ids = unique(agg))
      }
      indelning(cur)

      labels <- px_option_labels(values, t$variables$show[i] %||% NA)
      session$sendCustomMessage("pxSetOptions", list(
        id = ev$target,
        html = px_options_html(values$code, labels,
                               reverse = identical(t$variables$role[i], "time"),
                               common = px_common_of(values)),
        total = nrow(values)
      ))
    })

    fraga <- reactive({
      t <- tabell()
      req(t)
      v <- t$variables
      ind <- indelning()
      values <- t$values
      for (var in names(ind)) values[[var]] <- ind[[var]]$values
      selections <- lapply(seq_len(nrow(v)), function(i) {
        id <- var_id(t, "varden", i)
        vals <- input[[id]]
        # innan webbläsaren skickat något gäller förvalen
        if (is.null(vals) && is.null(input[[paste0(id, "_andrad")]])) {
          vals <- t$defaults[[v$code[i]]]
        }
        list(values = vals, latest = isTRUE(input[[paste0(id, "_senaste")]]),
             codelist = ind[[v$code[i]]]$id, codelist_type = ind[[v$code[i]]]$type)
      })
      names(selections) <- v$code
      q <- px_build_query(v, values, selections)
      q$query <- px_expand_aliases(q$query, t$aliases, indelade = names(ind))
      if (length(t$id) > 1 && length(ind) > 0) {
        valda_agg <- unlist(lapply(names(ind), function(k) {
          if (length(q$query[[k]]) == 1 && is.na(q$query[[k]])) return(NULL)
          ids <- ind[[k]]$agg_ids
          if (length(ids) > 0) stats::setNames(ids, rep(k, length(ids)))
        }))
        q$include_aggregations <- valda_agg
      }
      q$table_cells <- if (length(t$id) > 1) {
        koder <- lapply(ind, function(x) x$koder)
        celler <- px_table_cells(t$tables, q$query, indelade = koder)
        q$left_out <- attr(celler, "left_out")
        q$missing <- attr(celler, "missing")
        stats::setNames(as.numeric(celler), t$id)
      } else {
        stats::setNames(px_cell_count(v, q$query), t$id)
      }
      q$cells <- sum(q$table_cells)
      if (length(q$problems) == 0 && q$cells == 0) {
        q$problems <- "Ingen av tabellerna kan hämtas med de här valen."
      }
      # variabler som ska få en kodkolumn <variabel>_code; standard är bara
      # klartext. Variabler som elimineras finns inte i uttaget. Valen är
      # variablernas koder (t.ex. "Kon"), som är ASCII och lika på alla språk;
      # namnen (t.ex. "kön") visas bara i rutorna.
      valda <- input[[var_id(t, "kodvariabler", 0)]]
      med <- !vapply(q$query, function(x) length(x) == 1 && is.na(x), TRUE)
      q$included <- stats::setNames(v$code[med], v$label[med])
      q$code_variables <- unname(q$included[q$included %in% valda])
      q$code <- px_query_code(t$id, t$title, q$query, q$include_aggregations,
                              code_variables = q$code_variables,
                              lang = t$site$lang, base_url = t$site$base_url,
                              provider_name = t$site$name,
                              clean_names = isTRUE(input$clean_names),
                              provider = t$site$provider,
                              notes = px_table_notes(q$left_out, q$missing))
      q
    })

    output$clean_names_info <- renderUI({
      if (isTRUE(input$clean_names) && !requireNamespace("janitor", quietly = TRUE)) {
        div(class = "text-warning small mb-2",
            "Paketet janitor är inte installerat. Koden använder det, men data i",
            "gränssnittet får sina ursprungliga kolumnnamn. Installera med",
            tags$code('install.packages("janitor")'), ".")
      }
    })

    # bara variabler som finns med i uttaget kan väljas; byggs om när det
    # ändras, och behåller då de val som redan gjorts
    kod_valbara <- reactive({
      req(tabell())
      fraga()$included
    })

    output$kod_variabler_ui <- renderUI({
      t <- tabell()
      req(t)
      med <- kod_valbara()
      id <- var_id(t, "kodvariabler", 0)
      div(
        class = "px-kodval mb-2",
        checkboxGroupInput(
          id,
          label = tagList("Variabler som även ska få koder",
                          tags$small(class = "text-muted fw-normal",
                                     " (kolumnen får namnet variabel_code, övriga visas i klartext)")),
          choices = med, selected = intersect(isolate(input[[id]]), med), inline = TRUE
        )
      )
    })

    cell_text <- function(q) {
      txt <- paste0("Antal valda celler är: ",
                    format(q$cells, big.mark = " ", scientific = FALSE), ".")
      if (length(q$table_cells) > 1) {
        utan <- vapply(names(q$table_cells), function(id) {
          namn <- q$left_out[[id]]
          if (length(namn) > 0) paste0(" (utan ", paste(namn, collapse = ", "), ")") else ""
        }, "")
        saknas <- vapply(names(q$table_cells), function(id) {
          paste0("hämtas inte, inga giltiga värden valda för ", paste(q$missing[[id]], collapse = ", "))
        }, "")
        per_tabell <- paste0(sub("^.*/", "", names(q$table_cells)), ": ",
                             ifelse(q$table_cells == 0, saknas,
                                    paste0(format(q$table_cells, big.mark = " ", scientific = FALSE, trim = TRUE), utan)))
        txt <- paste0(txt, " Per tabell: ", paste(per_tabell, collapse = "; "), ".")
      }
      grans <- max_celler()
      if (max(q$table_cells) > grans) {
        txt <- paste0(txt, " Det är fler än gränsen på ",
                      format(grans, big.mark = " ", scientific = FALSE),
                      " celler per anrop, men pxweb2r delar automatiskt upp hämtningen i flera anrop.")
      }
      txt
    }

    output$cell_info <- renderUI({
      q <- fraga()
      tagList(
        p(class = "text-muted", cell_text(q)),
        if (!is.null(px_cell_warning(q$cells))) div(class = "alert alert-warning py-2 px-3 small", px_cell_warning(q$cells)),
        if (length(q$problems) > 0) div(class = "text-danger", lapply(q$problems, p))
      )
    })

    # ritas inne i "variabler", som ritas innan fliken visas: utan det här räknas
    # texten som dold och uppdateras inte
    outputOptions(output, "cell_info", suspendWhenHidden = FALSE)

    # --- Kod och data ----------------------------------------------------------
    output$fraga_status <- renderUI({
      if (is.null(tabell())) return(p("Välj först en tabell under flik 1."))
      q <- fraga()
      tagList(
        p(class = "text-muted", cell_text(q)),
        if (!is.null(px_cell_warning(q$cells))) div(class = "alert alert-warning py-2 px-3 small", px_cell_warning(q$cells)),
        if (length(q$problems) > 0) div(class = "text-danger", lapply(q$problems, p))
      )
    })

    output$kod <- renderText({
      req(tabell())
      fraga()$code
    })

    # sista utvägen för "Kopiera koden": appen körs lokalt, så urklippet i R
    # är användarens urklipp
    observeEvent(input$kopiera_via_server, {
      ok <- requireNamespace("clipr", quietly = TRUE) && clipr::clipr_available() &&
        !inherits(try(clipr::write_clip(fraga()$code), silent = TRUE), "try-error")
      session$sendCustomMessage("pxCopied", ok)
      if (!ok) {
        showNotification(paste("Kunde inte kopiera automatiskt. Markera koden och tryck Ctrl+C,",
                               "eller installera paketet clipr."), type = "warning")
      }
    })

    output$ladda_ner_skript <- downloadHandler(
      filename = function() paste0("hamta_", px_file_id(tabell()$id) %||% "tabell", ".R"),
      content = function(file) writeLines(fraga()$code, file, useBytes = TRUE)
    )

    data_resultat <- reactiveVal(NULL)
    data_fel <- reactiveVal(NULL)
    data_inaktuell <- reactiveVal(FALSE)
    # frågan som det hämtade datat (eller felet) gäller
    data_fraga <- reactiveVal(NULL)

    rensa_data <- function() {
      if (!is.null(isolate(data_fraga()))) data_inaktuell(TRUE)
      data_resultat(NULL)
      data_fel(NULL)
      data_fraga(NULL)
    }

    # allt som avgör vad som hämtas; kodkolumner och kolumnnamn styr bara vad som visas
    fraga_nu <- reactive({
      t <- tabell()
      req(t)
      q <- fraga()
      list(id = t$id, base_url = t$site$base_url, lang = t$site$lang, query = q$query,
           include_aggregations = q$include_aggregations,
           indelning = lapply(indelning(), function(x) x$id))
    })

    # tryCatch: ett fel i en observer skulle avsluta appen, och fraga() visar själv sina fel
    observe({
      req(data_fraga())
      nu <- tryCatch(fraga_nu(), error = function(e) NULL)
      if (!is.null(nu) && !identical(nu, data_fraga())) rensa_data()
    })

    observeEvent(input$hamta_data, {
      t <- tabell()
      if (is.null(t)) {
        showNotification("Välj först en tabell.", type = "warning")
        return()
      }
      q <- fraga()
      if (length(q$problems) > 0) {
        showNotification(paste(q$problems, collapse = " "), type = "warning")
        return()
      }
      # Kodkolumner hämtas för alla variabler i uttaget, så att rutorna kan visa och
      # dölja dem utan ny hämtning: frågan till API:et blir densamma.
      # flera tabeller: pxr_pxweb2_get_data() anropar varje tabell med dess egna värden
      tabeller <- if (length(t$id) > 1) t$id else t$meta
      res <- withProgress(message = paste("Hämtar data från", paste(t$id, collapse = ", ")),
                          value = 0.5, {
        tryCatch(px_get_data(backend, table = tabeller, query = q$query, quiet = TRUE,
                             base_url = t$site$base_url, lang = t$site$lang,
                             include_aggregations = q$include_aggregations %||% "none",
                             code_variables = unname(q$included)),
                 error = function(e) e)
      })
      data_inaktuell(FALSE)
      data_fraga(fraga_nu())
      if (inherits(res, "error")) {
        data_fel(conditionMessage(res))
        data_resultat(NULL)
      } else {
        data_fel(NULL)
        data_resultat(as.data.frame(res))
      }
    })

    output$data_status <- renderUI({
      if (!is.null(data_fel())) {
        return(div(class = "text-danger", "Hämtningen misslyckades: ", data_fel()))
      }
      d <- data_visad()
      if (is.null(d)) {
        return(p(class = "text-muted",
                 if (data_inaktuell()) {
                   "Valen har ändrats. Klicka på \"Hämta data\" för att hämta data på nytt."
                 } else {
                   "Ingen data hämtad ännu."
                 }))
      }
      p(class = "text-muted", sprintf("%s rader, %d kolumner.",
                                      format(nrow(d), big.mark = " "), ncol(d)))
    })

    # data som visas och laddas ner: med de valda kodkolumnerna, och med janitor-namn
    # om rutan är kryssad
    data_visad <- reactive({
      d <- data_resultat()
      if (is.null(d)) return(NULL)
      d <- px_keep_code_columns(d, tabell()$tables, fraga()$code_variables)
      if (isTRUE(input$clean_names)) px_clean_names(d) else d
    })

    # finns bara när det finns data: en DT-tabell töms inte när den blir utan innehåll
    output$data_tabell_ruta <- renderUI({
      req(data_resultat())
      DTOutput("data_tabell")
    })

    output$data_tabell <- renderDT({
      d <- data_visad()
      req(d)
      datatable(d, rownames = FALSE, filter = "top",
                options = list(pageLength = 25, scrollX = TRUE,
                               language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/sv-SE.json")))
    })

    # en rensning ska nå sidan direkt, även när fliken inte visas
    outputOptions(output, "data_status", suspendWhenHidden = FALSE)
    outputOptions(output, "data_tabell_ruta", suspendWhenHidden = FALSE)

    output$format_info <- renderUI({
      msg <- px_export_missing(input$format %||% "txt")
      if (!is.null(msg)) div(class = "text-warning small mb-2", msg)
    })

    output$ladda_ner_data <- downloadHandler(
      filename = function() paste0(px_file_id(tabell()$id) %||% "data", ".", input$format),
      content = function(file) {
        d <- data_visad()
        if (is.null(d)) d <- data.frame()
        px_write_data(d, file, input$format)
      }
    )
  }
}

# egen funktion så att avslutet går att testa
px_stop_app <- function(value = NULL) shiny::stopApp(value)

# egen funktion så att flikens läge går att testa
px_kommentar_flik <- function(session, aktiv) session$sendCustomMessage("pxKommentarFlik", aktiv)

#' Grafiskt gränssnitt för pxweb2r
#'
#' Öppnar ett fönster där du väljer statistikleverantör (SCB, SSB eller en
#' annan webbplats med PxWeb API v1 eller v2) och språk, bläddrar bland
#' ämnesområden och tabeller, väljer variabler och värden och får färdig
#' `pxr_pxweb2_get_data()`-kod med `lang` och `base_url`. Data
#' kan också hämtas direkt och laddas ner som txt, Excel eller parquet. Knappen
#' "Kommentarer" i flik 1 ger fliken "4. Kommentarer" med kommentarerna till de valda
#' tabellerna.
#'
#' En leverantör som har båda versionerna används alltid med API v2. Danmarks
#' Statistik (`"dst"`) har ett eget API, där "Välj indelning" saknas. Med API v1
#' saknas "Välj indelning" och avslutade tabeller går inte att skilja
#' ut, eftersom API:et saknar de uppgifterna. Tabellistan kan ta några minuter
#' att hämta första gången. Gränssnittet visar en upplysning om detta när en
#' sådan webbplats är vald.
#'
#' @param viewer Var fönstret visas: `"dialog"` (eget fönster i RStudio),
#'   `"pane"` (Viewer-panelen) eller `"browser"` (webbläsaren). I Positron
#'   visas appen i Positrons Viewer, om inte
#'   `options(pxweb2rgui.positron_viewer = ...)` anger något annat.
#' @param addin Om `TRUE` finns knappen "Infoga koden i skriptet och stäng",
#'   som skriver koden i det aktiva skriptet i RStudio.
#' @param provider Förvald statistikleverantör, `"scb"`, `"ssb"`, `"dst"`,
#'   `"statfin"`, `"fohm"`, `"sjv"`, `"tillvaxtanalys"`, `"konj"`, `"skolverket"`,
#'   `"vgr_arbetsmarknad"`, `"vgr_demografi"`, `"vgr_folkhalsa"`,
#'   `"vgr_gransregional"`, `"vgr_personal"` eller `"nordicstats"`. Standard är
#'   `getOption("pxweb2rgui.provider", "scb")`.
#'
#' @return Den genererade koden (osynligt) om den infogades, annars `NULL`.
#' @export
#' @example inst/examples/pxweb2_gui.R
pxweb2_gui <- function(viewer = c("dialog", "pane", "browser"), addin = FALSE,
                       provider = getOption("pxweb2rgui.provider", "scb")) {
  viewer <- match.arg(viewer)
  old <- options(pxweb2rgui.provider = provider)
  on.exit(options(old), add = TRUE)
  # dialog och pane kräver RStudio; annars webbläsaren. I Positron används
  # "browser", som Positron själv visar i sin Viewer (eller det som anges med
  # options(pxweb2rgui.positron_viewer = ...)).
  if (px_is_positron() && viewer != "browser") {
    viewer <- match.arg(getOption("pxweb2rgui.positron_viewer", "browser"),
                        c("browser", "pane", "dialog"))
  }
  if (viewer != "browser" && !px_rstudio_available()) viewer <- "browser"
  v <- switch(
    viewer,
    dialog  = shiny::dialogViewer("Statistikdatabaser (pxweb2r)", width = 1400, height = 950),
    pane    = shiny::paneViewer(minHeight = "maximize"),
    browser = shiny::browserViewer()
  )
  app <- shiny::shinyApp(px_app_ui(addin = addin), px_app_server(addin = addin))
  shiny::runGadget(app, viewer = v, stopOnCancel = FALSE)
}

#' RStudio-addin: statistiktabell till pxweb2r-kod
#'
#' Anropas från menyn Addins i RStudio. Samma som
#' `pxweb2_gui(viewer = "dialog", addin = TRUE)`.
#'
#' @return Den infogade koden (osynligt), eller `NULL` om fönstret stängdes
#'   utan att någon kod infogades.
#' @export
#' @example inst/examples/pxweb2_gui_addin.R
pxweb2_gui_addin <- function() {
  pxweb2_gui(viewer = "dialog", addin = TRUE)
}

# Skriver koden vid markören i det aktiva skriptet. Finns inget öppet skript
# skapas ett nytt. Utanför RStudio (eller utan paketet rstudioapi) skrivs
# koden ut i konsolen.
px_is_positron <- function() {
  identical(Sys.getenv("POSITRON"), "1") || nzchar(Sys.getenv("POSITRON_VERSION"))
}

px_rstudio_available <- function() {
  requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()
}

px_insert_code <- function(code) {
  if (!px_rstudio_available()) {
    cat(code)
    return(invisible(code))
  }
  ctx <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
  if (is.null(ctx) || is.null(ctx$id) || !nzchar(ctx$id)) {
    rstudioapi::documentNew(code, type = "r")
  } else {
    rstudioapi::insertText(text = code, id = ctx$id)
  }
  invisible(code)
}

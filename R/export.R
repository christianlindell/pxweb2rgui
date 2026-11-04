# Nedladdning av hämtad data i olika format.
# Excel och parquet kräver extra paket; finns inget av dem installerat visas
# formatet ändå, men med en uppmaning att installera paketet.

px_export_formats <- c(
  "Text (tabbseparerad, .txt)" = "txt",
  "Excel (.xlsx)"              = "xlsx",
  "Parquet (.parquet)"         = "parquet"
)

# Paket som kan skriva respektive format, i den ordning de provas
px_export_packages <- list(
  txt     = character(),
  xlsx    = c("writexl", "openxlsx"),
  parquet = c("nanoparquet", "arrow")
)

px_export_package <- function(format) {
  pkgs <- px_export_packages[[format]]
  if (length(pkgs) == 0) return("")
  hit <- pkgs[vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(hit) == 0) NA_character_ else hit[1]
}

# Text att visa om formatet inte går att skriva, annars NULL
px_export_missing <- function(format) {
  if (!is.na(px_export_package(format))) return(NULL)
  pkgs <- px_export_packages[[format]]
  paste0("Formatet kräver paketet ", pkgs[1], ". Installera med install.packages(\"",
         pkgs[1], "\") och starta om appen.")
}

px_write_data <- function(d, file, format) {
  d <- as.data.frame(d)
  pkg <- px_export_package(format)
  if (is.na(pkg)) stop(px_export_missing(format), call. = FALSE)
  switch(
    format,
    txt = utils::write.table(d, file, sep = "\t", row.names = FALSE, quote = FALSE,
                             na = "", fileEncoding = "UTF-8"),
    xlsx = if (pkg == "writexl") {
      writexl::write_xlsx(d, file)
    } else {
      openxlsx::write.xlsx(d, file)
    },
    parquet = if (pkg == "nanoparquet") {
      nanoparquet::write_parquet(d, file)
    } else {
      arrow::write_parquet(d, file)
    },
    stop("Okänt format: ", format, call. = FALSE)
  )
  invisible(file)
}

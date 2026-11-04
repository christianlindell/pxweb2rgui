# Samma som menyn Addins > "Statistiktabell till pxweb2r-kod" i RStudio.
# "Infoga koden i skriptet och stäng" skriver koden vid markören i det aktiva
# skriptet och returnerar den. Körs bara interaktivt och kräver internet.
if (interactive()) {
  kod <- pxweb2_gui_addin()
  if (!is.null(kod)) cat(kod)
}

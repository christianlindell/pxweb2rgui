# Gränssnittet är interaktivt: R väntar tills fönstret stängs. Därför körs
# exemplen bara i en interaktiv session. Appen hämtar tabellistan från
# leverantören och kräver internet.
if (interactive()) {
  # Enklaste anropet: SCB på svenska, i RStudio i ett eget fönster
  pxweb2_gui()

  # Börja med Statistisk sentralbyrå (SSB) i Norge och visa i webbläsaren
  pxweb2_gui(viewer = "browser", provider = "ssb")
}

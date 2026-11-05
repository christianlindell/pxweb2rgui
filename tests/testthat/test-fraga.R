vars <- data.frame(
  code = c("Region", "Kon", "ContentsCode", "Tid"),
  label = c("region", "kön", "tabellinnehåll", "år"),
  size = c(25L, 2L, 2L, 10L),
  elimination = c(TRUE, TRUE, FALSE, FALSE),
  role = c("geo", "other", "contents", "time"),
  stringsAsFactors = FALSE
)
vals <- list(
  Region = data.frame(code = c("00", sprintf("%02d", 1:24))),
  Kon = data.frame(code = c("1", "2")),
  ContentsCode = data.frame(code = c("N1", "N2")),
  Tid = data.frame(code = as.character(2015:2024))
)

test_that("valda värden översätts till pxweb2r:s query-syntax", {
  sel <- list(
    Region = list(values = c("20", "00")),
    Kon = list(values = NULL),
    ContentsCode = list(values = c("N2", "N1")),
    Tid = list(values = "2024", latest = TRUE)
  )
  q <- px_build_query(vars, vals, sel)
  # tabellens ordning, alla valda -> "*", inget valt -> NA, senaste -> "9999"
  expect_equal(q$query, list(Region = c("00", "20"), Kon = NA, ContentsCode = "*", Tid = "9999"))
  expect_length(q$problems, 0)
  expect_equal(px_cell_count(vars, q$query), 2 * 1 * 2 * 1)

  sel$Tid <- list(values = c("2023", "2024"))
  q <- px_build_query(vars, vals, sel)
  expect_equal(q$query$Tid, c("2023", "2024"))
  expect_equal(px_cell_count(vars, q$query), 2 * 1 * 2 * 2)
})

test_that("Välj alla värden ger * på alla variabler utom värdevariablerna", {
  pos <- px_mark_all_positions(vars)
  expect_identical(vars$code[pos], c("Region", "Kon", "Tid"))
  sel <- lapply(vals, function(d) list(values = d$code))
  sel$ContentsCode <- list(values = "N1")
  q <- px_build_query(vars, vals, sel)
  expect_equal(q$query, list(Region = "*", Kon = "*", ContentsCode = "N1", Tid = "*"))
  # utan känd värdevariabel gäller knappen alla variabler
  expect_length(px_mark_all_positions(transform(vars, role = NA_character_)), 4)
})

test_that("varning först när urvalet överskrider 1 miljon celler", {
  expect_null(px_cell_warning(403))
  expect_null(px_cell_warning(1e6))
  expect_match(px_cell_warning(1e6 + 1), "1 miljon celler", fixed = TRUE)
})

test_that("obligatoriska variabler utan val rapporteras", {
  q <- px_build_query(vars, vals, list(Region = list(values = "00")))
  expect_equal(q$problems, c("Välj minst ett värde för tabellinnehåll.",
                             "Välj minst ett värde för år."))
  expect_true(is.na(q$query$Kon))
})

test_that("förval följer SCB: första innehållet och senaste perioden", {
  expect_equal(px_default_selection("time", c("2023", "2024")), "2024")
  expect_equal(px_default_selection("contents", c("N1", "N2")), "N1")
  expect_length(px_default_selection("geo", c("00", "01")), 0)
})

test_that("listans klick känner av Ctrl, Cmd och Shift och har ett ankare", {
  # själva beteendet går bara att pröva i en webbläsare
  js <- pxweb2rgui:::px_var_js
  for (s in c("e.ctrlKey", "e.metaKey", "e.shiftKey", "pxAnkare")) expect_true(grepl(s, js, fixed = TRUE))
})

test_that("variabelrutan visar obligatoriskt, förval och tid i fallande ordning", {
  html <- as.character(px_var_box("v1", "år", data.frame(code = c("2023", "2024"),
                                  label = c("2023", "2024")), "time", FALSE))
  expect_match(html, "(Obligatoriskt)", fixed = TRUE)
  expect_match(html, paste0('<div class="px-opt sel" role="option" tabindex="-1" aria-selected="true"',
                            ' data-value="2024" title="2024">2024</div>',
                            '<div class="px-opt" role="option"'), fixed = TRUE)
  expect_match(html, "v1_senaste")
  reg <- as.character(px_var_box("v2", "region", data.frame(code = c("00", "0114"),
                                  label = c("Riket", "0114 Upplands Väsby")), "geo", TRUE,
                                  show = "code_value"))
  expect_false(grepl("Obligatoriskt", reg))
  expect_match(reg, ">00 Riket<")
  expect_match(reg, ">0114 Upplands Väsby<")
})

test_that("genererad kod går att tolka och ger samma fråga", {
  query <- list(Region = c("00", "20"), Kon = NA, ContentsCode = "*", Tid = "9999")
  code <- px_query_code("TAB6104", "Folkmängd", query)
  expect_match(code, 'table = "TAB6104"')
  call <- parse(text = code)[[2]]
  expect_equal(eval(call[[3]][[3]]), query)
  expect_match(px_query_code("X", "t", list(`ålder grupp` = "*")), "`ålder grupp` = \"\\*\"")
})

test_that("indelningar: värdemängd ger koder, aggregering ger include_aggregations", {
  lan <- vals
  lan$Region <- data.frame(code = c("01", "03"))
  sel <- list(Region = list(values = c("03", "01"), codelist = "vs_RegionLän07",
                            codelist_type = "Valueset"),
              ContentsCode = list(values = "N1"), Tid = list(values = "2024"))
  q <- px_build_query(vars, lan, sel)
  # alla län markerade blir ändå en uttrycklig lista, inte "*"
  expect_equal(q$query$Region, c("01", "03"))
  expect_null(q$include_aggregations)

  agg <- vals
  agg$Region <- data.frame(code = c("A1", "A2"))
  sel$Region <- list(values = "A1", codelist = "agg_RegionA-region", codelist_type = "Aggregation")
  q <- px_build_query(vars, agg, sel)
  expect_equal(q$query$Region, "A1")
  expect_equal(q$include_aggregations, c(Region = "agg_RegionA-region"))
  code <- px_query_code("TAB1", "t", q$query, q$include_aggregations)
  expect_match(code, 'include_aggregations = c(Region = "agg_RegionA-region")', fixed = TRUE)
  expect_silent(parse(text = code))
})

test_that("variabelrutan visar indelningsval när kodlistor finns", {
  cl <- data.frame(id = c("vs_RegionLän07", "agg_X"), label = c("Län", "X"),
                   type = c("Valueset", "Aggregation"))
  html <- as.character(px_var_box("v1", "region", data.frame(code = "00", label = "Riket"),
                                  "geo", TRUE, codelists = cl))
  expect_match(html, "px-indelning")
  expect_match(html, '<option value="vs_RegionLän07" data-type="Valueset">Län</option>')
})

test_that("obligatorisk variabel utan förval får röd ram från början", {
  kon <- data.frame(code = c("1", "2"), label = c("män", "kvinnor"))
  html <- as.character(px_var_box("v", "kön", kon, "other", FALSE))
  expect_match(html, "px-var px-var-required px-var-missing", fixed = TRUE)
  expect_match(html, "(Obligatoriskt)", fixed = TRUE)
  tid <- as.character(px_var_box("v", "år", data.frame(code = "2024", label = "2024"), "time", FALSE))
  expect_false(grepl("px-var-missing", tid))
  valfri <- as.character(px_var_box("v", "region", kon, "geo", TRUE))
  expect_false(grepl("px-var-required|Obligatoriskt", valfri))
})

test_that("px_r_value ger giltig R-kod även med citattecken och bakstreck", {
  v <- c("a\"b", "c\\d", "vanlig")
  expect_identical(eval(parse(text = px_r_value(v))), v)
  expect_identical(eval(parse(text = px_r_value("x\\y"))), "x\\y")
})

test_that("clean_names adds a janitor::clean_names() line to the code", {
  query <- list(Region = "20", Tid = "9999")
  expect_false(grepl("clean_names", px_query_code("TAB1", "t", query)))
  code <- px_query_code("TAB1", "t", query, clean_names = TRUE)
  expect_match(code, "dataset_df <- janitor::clean_names(dataset_df)", fixed = TRUE)
  expect_true(endsWith(code, "janitor::clean_names(dataset_df)\n"))
  expect_match(px_query_code("TAB1", "t", query, object_name = "x", clean_names = TRUE),
               "x <- janitor::clean_names(x)", fixed = TRUE)
})

test_that("px_clean_names uses janitor when it is installed", {
  d <- data.frame(`Folkmängd 2024` = 1, check.names = FALSE)
  if (requireNamespace("janitor", quietly = TRUE)) {
    expect_named(px_clean_names(d), "folkmangd_2024")
  } else {
    expect_identical(px_clean_names(d), d)
  }
})

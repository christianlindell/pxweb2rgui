test_that("gränssnittet behandlar tabell-id med bara siffror som text", {
  # jQuery:s .data() gör om "10531" till ett tal
  expect_false(grepl(".data('id')", px_tree_js, fixed = TRUE))
  b <- mock_backend()
  gm <- b$get_metadata
  id_in <- NULL
  b$get_metadata <- function(table_id, ...) { id_in <<- table_id; gm("TAB6104", ...) }
  old <- options(pxweb2rgui.backend = b, pxweb2rgui.cache_file = tempfile(fileext = ".rds"))
  on.exit(options(old))
  testServer(px_app_server(), {
    session$setInputs(sprak = "sv")
    session$setInputs(vald_tabell = 10531)
    expect_identical(id_in, "10531")
    expect_identical(tabell()$id, "10531")
    expect_match(fraga()$code, 'table = "10531"', fixed = TRUE)
  })
})

test_that(".pxweb2_check_table_id accepts reasonable ids", {
  expect_invisible(pxweb2rgui:::.pxweb2_check_table_id("TAB6104"))
  expect_identical(
    pxweb2rgui:::.pxweb2_check_table_id("statfin_vaerak_pxt_11ra"),
    "statfin_vaerak_pxt_11ra"
  )
})

test_that(".pxweb2_check_table_id rejects bad input", {
  expect_error(pxweb2rgui:::.pxweb2_check_table_id(NULL))
  expect_error(pxweb2rgui:::.pxweb2_check_table_id(NA_character_))
  expect_error(pxweb2rgui:::.pxweb2_check_table_id(""))
  expect_error(pxweb2rgui:::.pxweb2_check_table_id(c("TAB1", "TAB2")))
  expect_error(pxweb2rgui:::.pxweb2_check_table_id(6104L))
  expect_error(
    pxweb2rgui:::.pxweb2_check_table_id(
      "https://statistikdatabasen.scb.se/api/v2/tables/TAB6104"
    )
  )
  expect_error(pxweb2rgui:::.pxweb2_check_table_id("TAB 6104"))
})

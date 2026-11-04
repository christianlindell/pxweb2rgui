d <- data.frame(region = c("Stockholms län", "Uppsala län"), år = c("2023", "2024"),
                värde = c(2454821, NA))

test_that("txt blir tabbseparerad UTF-8 med svenska tecken", {
  f <- tempfile(fileext = ".txt")
  px_write_data(d, f, "txt")
  lines <- readLines(f, encoding = "UTF-8")
  expect_equal(lines[1], "region\tår\tvärde")
  expect_equal(lines[2], "Stockholms län\t2023\t2454821")
  expect_equal(lines[3], "Uppsala län\t2024\t")
})

test_that("excel går att läsa tillbaka", {
  skip_if(is.na(px_export_package("xlsx")))
  f <- tempfile(fileext = ".xlsx")
  px_write_data(d, f, "xlsx")
  back <- openxlsx::read.xlsx(f)
  expect_equal(names(back), names(d))
  expect_equal(back$värde[1], 2454821)
})

test_that("parquet går att läsa tillbaka", {
  pkg <- px_export_package("parquet")
  skip_if(is.na(pkg), "varken nanoparquet eller arrow installerat")
  f <- tempfile(fileext = ".parquet")
  px_write_data(d, f, "parquet")
  back <- if (pkg == "nanoparquet") nanoparquet::read_parquet(f) else arrow::read_parquet(f)
  expect_equal(as.data.frame(back), d)
})

test_that("saknat paket ger ett begripligt fel", {
  local_mocked_bindings(px_export_package = function(format) NA_character_)
  expect_error(px_write_data(d, tempfile(), "parquet"),
               'install.packages("nanoparquet")', fixed = TRUE)
})

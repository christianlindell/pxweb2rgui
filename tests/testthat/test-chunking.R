test_variables_df <- function(sizes, roles = NULL) {
  if (is.null(roles)) roles <- rep("other", length(sizes))
  tibble::tibble(
    code = names(sizes),
    label = tolower(names(sizes)),
    size = as.integer(sizes),
    elimination = FALSE,
    role = roles,
    show = "value"
  )
}

wildcard_query <- function(vars) {
  list(selection = purrr::map(vars, ~ list(variableCode = .x, valueCodes = list("*"))))
}

test_that(".pxweb2_count_cells handles wildcards, explicit codes and top/bottom", {
  vars <- test_variables_df(c(Region = 10L, Tid = 5L))

  expect_equal(pxweb2rgui:::.pxweb2_count_cells(vars, wildcard_query(c("Region", "Tid"))), 50)

  q <- list(selection = list(
    list(variableCode = "Region", valueCodes = list("01", "02", "03")),
    list(variableCode = "Tid", valueCodes = list("*"))
  ))
  expect_equal(pxweb2rgui:::.pxweb2_count_cells(vars, q), 15)

  q_topn <- list(selection = list(
    list(variableCode = "Region", valueCodes = list("top(2)")),
    list(variableCode = "Tid", valueCodes = list("*"))
  ))
  expect_equal(pxweb2rgui:::.pxweb2_count_cells(vars, q_topn), 10)
})

test_that(".pxweb2_choose_split_variable prefers geo but falls back to the largest variable", {
  vars_med_geo <- test_variables_df(
    c(Region = 50L, Bransch = 500L, Tid = 10L, ContentsCode = 2L),
    roles = c("geo", "other", "time", "contents")
  )
  expect_identical(
    pxweb2rgui:::.pxweb2_choose_split_variable(vars_med_geo, wildcard_query(vars_med_geo$code)),
    "Region"
  )

  # No geo variable at all - the largest non-time/non-contents variable is used instead
  vars_utan_geo <- test_variables_df(
    c(A = 5000L, B = 10L, Tid = 5L, ContentsCode = 2L),
    roles = c("other", "other", "time", "contents")
  )
  expect_identical(
    pxweb2rgui:::.pxweb2_choose_split_variable(vars_utan_geo, wildcard_query(vars_utan_geo$code)),
    "A"
  )
})

test_that(".pxweb2_make_chunks splits a table without a geo variable into chunks under max_cells", {
  vars <- test_variables_df(
    c(A = 5000L, B = 10L, Tid = 5L, ContentsCode = 2L),
    roles = c("other", "other", "time", "contents")
  )
  valid_values_list <- list(
    A = tibble::tibble(code = sprintf("a%04d", 1:5000), type = "Variable")
  )

  chunks <- pxweb2rgui:::.pxweb2_make_chunks(vars, wildcard_query(vars$code), valid_values_list, max_cells = 150000)

  expect_gt(length(chunks), 1)
  celler <- sapply(chunks, function(q) pxweb2rgui:::.pxweb2_count_cells(vars, q))
  expect_true(all(celler <= 150000))
  # alla A-koder ska finnas med totalt, exakt en gång
  alla_a <- unlist(lapply(chunks, function(q) {
    sel <- q$selection[[which(sapply(q$selection, `[[`, "variableCode") == "A")]]
    unlist(sel$valueCodes)
  }))
  expect_identical(sort(alla_a), sort(valid_values_list$A$code))
})

test_that(".pxweb2_make_chunks splits on a second variable when one is not enough", {
  # Samma situation som TAB5693: region ensam räcker inte för att komma under max_cells,
  # eftersom de övriga dimensionerna redan på egen hand ger fler celler än gränsen.
  # Då delas varje region vidare på SNI2007.
  vars <- test_variables_df(
    c(Region = 3L, SNI2007 = 1582L, Tid = 17L, ContentsCode = 42L),
    roles = c("geo", "other", "time", "contents")
  )
  valid_values_list <- list(
    Region = tibble::tibble(code = sprintf("%04d", 1:3), type = "Variable"),
    SNI2007 = tibble::tibble(code = sprintf("s%04d", 1:1582), type = "Variable")
  )

  chunks <- pxweb2rgui:::.pxweb2_make_chunks(vars, wildcard_query(vars$code), valid_values_list,
                                          max_cells = 150000)
  celler <- sapply(chunks, function(q) pxweb2rgui:::.pxweb2_count_cells(vars, q))
  expect_true(all(celler <= 150000))
  expect_equal(sum(celler), 3 * 1582 * 17 * 42)
  # nära det minsta möjliga antalet anrop
  expect_lte(length(chunks), 3 * ceiling(1582 * 17 * 42 / 150000))
})

test_that(".pxweb2_make_chunks stops with an informative message when nothing is left to split", {
  vars <- test_variables_df(c(A = 1L, B = 1L), roles = c("other", "other"))
  q <- list(selection = list(
    list(variableCode = "A", valueCodes = list("a")),
    list(variableCode = "B", valueCodes = list("b"))
  ))
  expect_error(
    pxweb2rgui:::.pxweb2_make_chunks(vars, q, list(), max_cells = 0),
    "Too many cells selected"
  )
})

test_that("code lists go into the POST body, outputValues stays a query parameter", {
  req <- pxweb2rgui:::.pxweb2_request(
    body = list(selection = list(
      list(variableCode = "Region", valueCodes = list("*")),
      list(variableCode = "Tid", valueCodes = list("2024"))
    )),
    extra_query = list(`codelist[Region]` = "agg_RegionA-region",
                       `outputValues[Region]` = "aggregated")
  )
  post <- pxweb2rgui:::.pxweb2_post_request(req)
  expect_identical(post$body$selection[[1]]$codelist, "agg_RegionA-region")
  expect_null(post$body$selection[[2]]$codelist)
  expect_identical(post$query, list(`outputValues[Region]` = "aggregated"))

  json <- as.character(jsonlite::toJSON(post$body, auto_unbox = TRUE))
  expect_match(json, '"variableCode":"Region","valueCodes":["*"],"codelist":"agg_RegionA-region"', fixed = TRUE)

  plain <- pxweb2rgui:::.pxweb2_post_request(pxweb2rgui:::.pxweb2_request(body = req$body))
  expect_identical(plain$body, req$body)
  expect_length(plain$query, 0)
})

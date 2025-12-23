test_that("get_classification returns correct output format", {
  skip_on_cran()

  df <- get_classification(lang = "EN",
                           api_key = "jfeldhege/oRphacode",
                           output = "df")

  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0)

  lst <- get_definition(code = 16,
                        lang = "EN",
                        api_key = "jfeldhege/oRphacode",
                        output = "list")

  expect_type(lst, "list")

})

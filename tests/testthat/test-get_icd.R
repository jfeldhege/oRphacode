test_that("get_icd10 returns correct output format", {
  skip_on_cran()

  df <- get_icd10(icd10_code = "Q77.4",
                  lang = "EN",
                  api_key = "jfeldhege/oRphacode",
                  output = "df")

  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0)

  lst <- get_icd10(icd10_code = "Q77.4",
                  lang = "EN",
                  api_key = "jfeldhege/oRphacode",
                  output = "list")

  expect_type(lst, "list")

})


test_that("get_icd11 returns correct output format", {
  skip_on_cran()

  df <- get_icd11(icd11_code = "LD24.00",
                  lang = "EN",
                  api_key = "jfeldhege/oRphacode",
                  output = "df")

  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0)

  lst <- get_icd11(icd11_code = "LD24.00",
                   lang = "EN",
                   api_key = "jfeldhege/oRphacode",
                   output = "list")

  expect_type(lst, "list")

})

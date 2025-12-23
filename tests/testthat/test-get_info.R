test_that("get_status returns correct output format", {
  skip_on_cran()

  df <- get_status(code = 16,
                   lang = "EN",
                   api_key = "jfeldhege/oRphacode",
                   output = "df")

  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0)

  lst <- get_status(code = 16,
                    lang = "EN",
                    api_key = "jfeldhege/oRphacode",
                    output = "list")

  expect_type(lst, "list")

})

test_that("get_definition returns correct output format", {
  skip_on_cran()

  df <- get_definition(code = 16,
                       lang = "EN",
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

test_that("get_typology returns correct output format", {
  skip_on_cran()

  df <- get_typology(code = 16,
                     lang = "EN",
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


test_that("get_summary returns correct output format", {
  skip_on_cran()

  df <- get_summary(code = 16,
                    lang = "EN",
                    api_key = "jfeldhege/oRphacode",
                    output = "df")

  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0)

  lst <- get_summary(code = 16,
                     lang = "EN",
                     api_key = "jfeldhege/oRphacode",
                     output = "list")

  expect_type(lst, "list")

})

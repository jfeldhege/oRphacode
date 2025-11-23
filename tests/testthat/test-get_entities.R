test_that("get_entities returns a data frame", {
  skip_on_cran()

  df <- get_entities(lang = "EN",
                     api_key = "jfeldhege")

  expect_s3_class(df, "data.frame")

  expect_true(nrow(df) > 0)

  # Expected columns
  expect_true(all(c("Status", "Preferred term", "ORPHAcode", "Definition", "Date") %in% names(df)))
})

test_that("dataframe is valid", {
  expect_true(is.list(summary_stats()))
  expect_equal(class(summary_stats()), "data.frame")
})

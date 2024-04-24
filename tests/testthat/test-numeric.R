test_that("dataframe is valid", {
  expect_true(is.list(summary_stats(mtcars)))
  expect_equal(class(summary_stats(mtcars, mtcars$mpg)), "data.frame")
})

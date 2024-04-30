test_that("dataframe is valid", {
  expect_true(is.list(summary_stats.numeric(mtcars$mpg, mtcars)))
})

test_that("summary_stats is valid", {
  expect_true(is.list(summary_stats.factor(mtcars$gear, mtcars)))
})

test_that("distributions work", {
  vdiffr::expect_doppelganger(
    title = "mtcars mpg",
    fig = distribution.numeric(mtcars$mpg, mtcars)
  )
})


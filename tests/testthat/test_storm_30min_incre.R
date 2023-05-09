library(testthat)
test_that("Check if the expanded 30-min-interval dataframe of the storm AL061851
          has the same row number as wanted", {
  data("hurdat")
  result <- storm_30min_incre("AL061851")
  expect_equal(nrow(result), 192)
})

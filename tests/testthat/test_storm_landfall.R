test_that("Check if the output of storm_landfall() on the storm AL061851 has the
          same dimension and same result as expected", {
  data("hurdat")
  result <- storm_landfall("AL061851")
  expect_equal(length(result), 1)
  expect_equal(result, TRUE)
})

test_that("Check if the output of storm_landfall() on the storm AL061851 and
AL011851 has the same dimension and same result as expected", {
  data("hurdat")
  result <- storm_landfall(c("AL061851", "AL011851"))
  expect_equal(length(result), 2)
  expect_equal(result, c(TRUE, TRUE))
})

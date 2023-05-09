test_that("Check if the storm_map() on storm AL172022 generates a plot", {
  data("hurdat")
  storm_map("AL172022")
  expect_true(!is.null(last_plot()))
  expect_true(inherits(last_plot(), "ggplot"))
})

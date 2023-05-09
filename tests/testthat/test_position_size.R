test_that("Check if the position_size() on storm AL172022 generates a plot", {
  data("hurdat")
  position_size("AL172022", "20221109", "1800")
  expect_true(!is.null(last_plot()))
  expect_true(inherits(last_plot(), "ggplot"))
})

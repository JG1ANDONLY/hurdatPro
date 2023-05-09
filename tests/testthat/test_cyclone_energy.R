test_that("Check if the accumulated cyclone energy of the storm AL061851 has the
          same dimension and value as expected", {
  data("hurdat")
  result <- cyclone_energy("AL061851")
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), 2)
  expect_equal(colnames(result), c("stormid", "ace_energy"))
  expect_equal(result$stormid[1], "AL061851")
  expect_equal(result$ace_energy[1], 4.61)
})

test_that("Check if the accumulated cyclone energy of the storm AL061851 and
AL011851 has the same dimension and value as expected", {
  data("hurdat")
  result <- cyclone_energy(c("AL061851", "AL011851"))
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 2)
  expect_equal(colnames(result), c("stormid", "ace_energy"))
  expect_equal(result$stormid[1], "AL061851")
  expect_equal(result$stormid[2], "AL011851")
  expect_equal(result$ace_energy[1], 4.61)
  expect_equal(result$ace_energy[2], 4.91)
})


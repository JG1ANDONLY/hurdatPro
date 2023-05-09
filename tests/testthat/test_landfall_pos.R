test_that("Check if the landfall position of AL061851 made a landfall within the
          border of the United States on 1851/10/19 at 18:00", {
  data("hurdat")
  result <- landfall_pos("AL061851", "18511019", "1800")
  expect_equal(result, TRUE)
})

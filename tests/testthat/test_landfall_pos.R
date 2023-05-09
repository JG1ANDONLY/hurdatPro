library(testthat)
test_that("Check if the landfall position of ", {
            data("hurdat")
            result <- storm_30min_incre("AL061851")
            expect_equal(nrow(result), 192)
          })

test_that("p value rounding output is character", {
  expect_type(round_p(runif(100, min = 0, max = 1)), "character")
})

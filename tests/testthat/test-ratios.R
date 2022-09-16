test_that("primary octave of ratios matches expectations", {
  expect_equal(ratios$up.numerator,c(1,16,9,6,5,4,7,3,8,5,16,15,2))
  expect_equal(ratios$up.denominator,c(1,15,8,5,4,3,5,2,5,3, 9, 8,1))
})

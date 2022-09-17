test_that("up ratios within the primary octave match expectations", {
  expect_equal(ratios()$up.numerator,  c(1,16,9,6,5,4,7,3,8,5,16,15,2))
  expect_equal(ratios()$up.denominator,c(1,15,8,5,4,3,5,2,5,3, 9, 8,1))
})
test_that('up ratio within and beyond the primary octave matches expectations', {
  expect_equal(ratio(7,1)[['numerator']],3)
  expect_equal(ratio(7,1)[['denominator']],2)

  expect_equal(ratio(12,1)[['numerator']],2)
  expect_equal(ratio(12,1)[['denominator']],1)

  expect_equal(ratio(13,1)[['numerator']],32)
  expect_equal(ratio(13,1)[['denominator']],15)

  expect_equal(ratio(-1,1)[['numerator']],15)
  expect_equal(ratio(-1,1)[['denominator']],16)

  expect_equal(ratio(-13,1)[['numerator']],15)
  expect_equal(ratio(-13,1)[['denominator']],32)

  expect_equal(ratio(-25,1)[['numerator']],15)
  expect_equal(ratio(-25,1)[['denominator']],64)

  expect_equal(ratio(-37,1)[['numerator']],15)
  expect_equal(ratio(-37,1)[['denominator']],128)
})
test_that("down ratios within the primary octave match expectations", {
  expect_equal(ratios()$down.numerator,  c(1, 8, 9,3,5,2,5,3,4,5,8,15,1))
  expect_equal(ratios()$down.denominator,c(2,15,16,5,8,3,7,4,5,6,9,16,1))
})
test_that('down ratio within and beyond the primary octave matches expectations', {
  expect_equal(ratio(7,-1)[['numerator']],3)
  expect_equal(ratio(7,-1)[['denominator']],4)

  expect_equal(ratio(12,-1)[['numerator']],1)
  expect_equal(ratio(12,-1)[['denominator']],1)

  expect_equal(ratio(13,-1)[['numerator']],16)
  expect_equal(ratio(13,-1)[['denominator']],15)

  expect_equal(ratio(25,-1)[['numerator']],32)
  expect_equal(ratio(25,-1)[['denominator']],15)

  expect_equal(ratio(37,-1)[['numerator']],64)
  expect_equal(ratio(37,-1)[['denominator']],15)

  expect_equal(ratio(-1,-1)[['numerator']],15)
  expect_equal(ratio(-1,-1)[['denominator']],32)

  expect_equal(ratio(-13,-1)[['numerator']],15)
  expect_equal(ratio(-13,-1)[['denominator']],64)

  expect_equal(ratio(-25,-1)[['numerator']],15)
  expect_equal(ratio(-25,-1)[['denominator']],128)
})

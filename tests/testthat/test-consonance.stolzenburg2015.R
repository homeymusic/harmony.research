test_that('modified relative periodicity is symmetrical',{
  expect_equal(consonance.stolzenburg2015(c(0,4))$affinity,
               consonance.stolzenburg2015(c(0,8))$affinity)
  expect_equal(consonance.stolzenburg2015(c(0,4))$brightness,
               -consonance.stolzenburg2015(c(0,8))$brightness)
})

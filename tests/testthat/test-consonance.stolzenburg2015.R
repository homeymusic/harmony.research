test_that('modified relative periodicity is symmetrical',{
  M3_up = consonance.stolzenburg2015(c(0,4))
  m6_up = consonance.stolzenburg2015(c(0,8))
  expect_equal(M3_up$affinity,m6_up$affinity)
  expect_equal(M3_up$brightness,-m6_up$brightness)

  M3_down = consonance.stolzenburg2015(c(0,4),-1)
  m6_down = consonance.stolzenburg2015(c(0,8),-1)
  expect_equal(M3_down$affinity, m6_down$affinity)
  expect_equal(M3_down$brightness,-m6_down$brightness)
})

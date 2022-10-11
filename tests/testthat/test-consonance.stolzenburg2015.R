test_that('modified relative periodicity of pitchs is symmetrical',{
  M3 = h(c(4))
  m6 = h(c(8))
  expect_equal(M3$stolzenburg2015.affinity,m6$stolzenburg2015.affinity)
  expect_equal(M3$stolzenburg2015.brightness,-m6$stolzenburg2015.brightness)
})
test_that('modified relative periodicity of tonic dyads is symmetrical',{
  M3_up = h(c(0,4),1)
  m6_up = h(c(0,8),1)
  expect_equal(M3_up$stolzenburg2015.affinity,m6_up$stolzenburg2015.affinity)
  expect_equal(M3_up$stolzenburg2015.brightness,-m6_up$stolzenburg2015.brightness)

  M3_down = h(c(0,4),-1)
  m6_down = h(c(0,8),-1)
  expect_equal(M3_down$stolzenburg2015.affinity, m6_down$stolzenburg2015.affinity)
  expect_equal(M3_down$stolzenburg2015.brightness,-m6_down$stolzenburg2015.brightness)

  expect_equal(M3_down$stolzenburg2015.brightness,-M3_up$stolzenburg2015.brightness)
  expect_equal(m6_down$stolzenburg2015.brightness,-m6_up$stolzenburg2015.brightness)
})
test_that('modified relative periodicity of octave dyads is symmetrical',{
  inv_M3_up = h(c(4,12),1)
  inv_m6_up = h(c(8,12),1)
  expect_equal(inv_M3_up$stolzenburg2015.affinity,inv_m6_up$stolzenburg2015.affinity)
  expect_equal(inv_M3_up$stolzenburg2015.brightness,-inv_m6_up$stolzenburg2015.brightness)

  inv_M3_down = h(c(4,12),-1)
  inv_m6_down = h(c(8,12),-1)
  expect_equal(inv_M3_down$stolzenburg2015.affinity, inv_m6_down$stolzenburg2015.affinity)
  expect_equal(inv_M3_down$stolzenburg2015.brightness,-inv_m6_down$stolzenburg2015.brightness)

  expect_equal(inv_M3_down$stolzenburg2015.brightness,-inv_M3_up$stolzenburg2015.brightness)
  expect_equal(inv_m6_down$stolzenburg2015.brightness,-inv_m6_up$stolzenburg2015.brightness)
})

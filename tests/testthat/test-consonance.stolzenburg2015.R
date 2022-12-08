test_that('modified relative periodicity of pitchs is symmetrical',{
  M3 = h(c(4))
  m6 = h(c(8))
  expect_equal(M3$stolzenburg2015.consonance,m6$stolzenburg2015.consonance)
  expect_equal(M3$stolzenburg2015.brightness,-m6$stolzenburg2015.brightness)
})
test_that('modified relative periodicity of tonic dyads is symmetrical',{
  M3_up = h(c(0,4),0)
  m6_up = h(c(0,8),0)
  expect_equal(M3_up$stolzenburg2015.consonance,m6_up$stolzenburg2015.consonance)
  expect_equal(M3_up$stolzenburg2015.brightness,-m6_up$stolzenburg2015.brightness)

  M3_down = h(c(0,4),12)
  m6_down = h(c(0,8),12)
  expect_equal(M3_down$stolzenburg2015.consonance, m6_down$stolzenburg2015.consonance)
  expect_equal(M3_down$stolzenburg2015.brightness,-m6_down$stolzenburg2015.brightness)

  expect_equal(M3_down$stolzenburg2015.brightness,-M3_up$stolzenburg2015.brightness)
  expect_equal(m6_down$stolzenburg2015.brightness,-m6_up$stolzenburg2015.brightness)
})
test_that('modified relative periodicity of octave dyads is symmetrical',{
  inv_M3_up = h(c(4,12),0)
  inv_m6_up = h(c(8,12),0)
  expect_equal(inv_M3_up$stolzenburg2015.consonance,inv_m6_up$stolzenburg2015.consonance)
  expect_equal(inv_M3_up$stolzenburg2015.brightness,-inv_m6_up$stolzenburg2015.brightness)

  inv_M3_down = h(c(4,12),12)
  inv_m6_down = h(c(8,12),12)
  expect_equal(inv_M3_down$stolzenburg2015.consonance, inv_m6_down$stolzenburg2015.consonance)
  expect_equal(inv_M3_down$stolzenburg2015.brightness,-inv_m6_down$stolzenburg2015.brightness)

  expect_equal(inv_M3_down$stolzenburg2015.brightness,-inv_M3_up$stolzenburg2015.brightness)
  expect_equal(inv_m6_down$stolzenburg2015.brightness,-inv_m6_up$stolzenburg2015.brightness)
})
test_that('modified relative periodicity of tonic triads is up-down symmetrical',{
  M_up = h(c(0,4,7),0)
  M_down = h(-c(0,4,7),12)
  expect_equal(M_up$stolzenburg2015.brightness,-M_down$stolzenburg2015.brightness)
  expect_equal(M_up$stolzenburg2015.consonance,M_down$stolzenburg2015.consonance)
  m_up = h(c(0,3,7),0)
  m_down = h(-c(0,3,7),12)
  expect_equal(m_up$stolzenburg2015.brightness,-m_down$stolzenburg2015.brightness)
  expect_equal(m_up$stolzenburg2015.consonance,m_down$stolzenburg2015.consonance)
})

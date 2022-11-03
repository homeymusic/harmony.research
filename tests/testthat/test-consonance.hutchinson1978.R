test_that('rougness of major and minor pitches is asymmetrical',{
  M3 = h(c(4))
  m6 = h(c(8))
  expect_lt(M3$hutchinson1978.affinity,m6$hutchinson1978.affinity)
  expect_lt(M3$hutchinson1978.brightness,-m6$hutchinson1978.brightness)
})

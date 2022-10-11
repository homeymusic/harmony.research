test_that("interval affinity behaves well",{
  expect_equal(consonance.primes(0)$affinity,consonance.primes(12)$affinity)
  purrr::pmap(interval_components(),~expect_equal(consonance.primes(..1)$affinity,..4,info=paste('interval:',..1,'affinity',..4)))
})
test_that("interval brightness behaves well",{
  purrr::pmap(interval_components(),~expect_equal(consonance.primes(..1)$brightness,..3))
})
test_that("dissonance measure matches expectations", {
  expected_tonic_primes = c(0,16,12,10,9,7,12,5,11,8,14,14,2)
  diss = purrr::map_dbl(interval_components()$integer_position,~consonance.primes(.x)$tonic.dissonance)
  expect_equal(diss, expected_tonic_primes)

  expected_octave_primes = c(2,14,14,8,11,5,12,7,9,10,12,16,0)
  diss = purrr::map_dbl(interval_components()$integer_position,~consonance.primes(.x)$octave.dissonance)
  expect_equal(diss, expected_octave_primes)
})
test_that('upper bound of dissonance makes sense',{
  expect_equal(consonance.primes.max_dissonance(),16)
})
test_that('octave complements match as expected',{
  expect_equal(consonance.primes(7)$tonic.dissonance %>% unname,
               consonance.primes(5)$octave.dissonance %>% unname)
  expect_equal(consonance.primes(4)$tonic.dissonance %>% unname,
               consonance.primes(8)$octave.dissonance %>% unname)
  expect_equal(consonance.primes(12)$tonic.dissonance %>% unname,
               consonance.primes(0)$octave.dissonance %>% unname)
})
test_that('primes consonance of pitches is symmetrical',{
  M3 = h(c(4))
  m6 = h(c(8))
  expect_equal(M3$primes.affinity,m6$primes.affinity)
  expect_equal(M3$primes.brightness,-m6$primes.brightness)
})
test_that('primes consonance of tonic dyads is symmetrical',{
  M3_up = h(c(0,4),+1)
  m6_down = h(c(8,12),-1)
  expect_equal(M3_up$primes.affinity,m6_down$primes.affinity)
  expect_equal(M3_up$primes.brightness,-m6_down$primes.brightness)

  M3_down = h(c(0,4),-1)
  m6_up = h(c(8,12),+1)
  expect_equal(M3_down$primes.affinity, m6_up$primes.affinity)
  expect_equal(M3_down$primes.brightness,-m6_up$primes.brightness)

  expect_equal(M3_down$primes.brightness,-M3_up$primes.brightness)
  expect_equal(m6_down$primes.brightness,-m6_up$primes.brightness)
})
test_that('tonic triads are up-down symmetrical',{
  M_up = h(c(0,4,7),+1)
  M_down = h(-c(0,4,7),-1)
  expect_equal(M_up$primes.brightness,-M_down$primes.brightness)
  expect_equal(M_up$primes.affinity,M_down$primes.affinity)
  m_up = h(c(0,3,7),+1)
  m_down = h(-c(0,3,7),-1)
  expect_equal(m_up$primes.brightness,-m_down$primes.brightness)
  expect_equal(m_up$primes.affinity,m_down$primes.affinity)
})

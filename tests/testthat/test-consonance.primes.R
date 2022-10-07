test_that("interval affinity behaves well",{
  expect_equal(consonance.primes(0)$affinity,consonance.primes(12)$affinity)
  purrr::pmap(intervals(),~expect_equal(consonance.primes(..1)$affinity,..4,info=paste('interval:',..1,'affinity',..4)))
})
test_that("interval brightness behaves well",{
  purrr::pmap(intervals(),~expect_equal(consonance.primes(..1)$brightness,..3))
})
test_that("dissonance measure matches expectations", {
  expected_tonic_primes = c(0,16,12,10,9,7,12,5,11,8,14,14,2)
  diss = purrr::map_dbl(intervals()$integer_position,~consonance.primes(.x)$tonic.dissonance)
  expect_equal(diss, expected_tonic_primes)

  expected_octave_primes = c(2,14,14,8,11,5,12,7,9,10,12,16,0)
  diss = purrr::map_dbl(intervals()$integer_position,~consonance.primes(.x)$octave.dissonance)
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

test_that("interval affinity behaves well",{
  expect_equal(p(0)$affinity,p(12)$affinity)
  purrr::pmap(intervals(),~expect_equal(p(..1)$affinity,..4,info=paste('interval:',..1,'affinity',..4)))
})
test_that("interval brightness behaves well",{
  purrr::pmap(intervals(),~expect_equal(p(..1)$brightness,..3))
})
test_that("dissonance measure matches expectations", {
  expected_tonic_primes = c(0,16,12,10,9,7,12,5,11,8,14,14,2)
  purrr::pmap(intervals(),~expect_equal(
    p(..1)$tonic.dissonance,
    expected_tonic_primes[..1+1],
    info=paste('integer_position:',..1,..2,'probe.freq:',compound_ratios(..1,1),'ref.freq:',compound_ratios(..1,1))
  ))
  expected_octave_primes = c(2,14,14,8,11,5,12,7,9,10,12,16,0)
  intervals() %>% purrr::pmap(~expect_equal(
    p(..1)$octave.dissonance,
    expected_octave_primes[..1+1],
    info=paste('integer_position:',..1,..2,'probe.freq:',compound_ratios(..1,1),'ref.freq',compound_ratios(..1,1))
  ))
})
test_that('upper bound of dissonance makes sense',{
  expect_equal(max_dissonance(),16)
})
test_that('brightness and affinity are symmetrical with symmetrical chords',{
  expect_equal(h(c(0,4,7))$affinity,h(-c(0,4,7),-1)$affinity)
  expect_equal(h(c(0,4,7))$brightness,-h(-c(0,4,7),-1)$brightness)

  expect_equal(h(c(0,3,7))$affinity,h(-c(0,3,7),-1)$affinity)
  expect_equal(h(c(0,3,7))$brightness,-h(-c(0,3,7),-1)$brightness)
})
test_that('the major triad is perfectly bright. and the minor triad is a third', {
  expect_equal(major_triad_root$brightness,1)
  expect_equal(minor_triad_root$brightness,1/3,tolerance=0.00001)
  # symetrical triads are interesting as well
  expect_equal(h(c(0,4,7,12))$brightness,0.5)
  expect_equal(h(c(0,3,7,12))$brightness,0.0)
})
test_that('the similarities among major and minor triads under inversion are interesting',{
  expect_equal(major_triad_root$affinity,major_triad_first_inversion$affinity)
  expect_equal(major_triad_first_inversion$brightness,major_triad_second_inversion$brightness)

  expect_equal(minor_triad_root$affinity,minor_triad_first_inversion$affinity)
  expect_equal(minor_triad_first_inversion$brightness,minor_triad_second_inversion$brightness)
})
test_that("tonic-octave symmetrical chords have identical consonance regardless of direction",{
  chord = c(0,4,7,12)
  expect_equal(h(chord,direction=0)$affinity,h(chord,direction=+1)$affinity)
  expect_equal(h(chord,direction=0)$brightness,h(chord,direction=+1)$brightness)
  expect_equal(h(chord,direction=0)$affinity,h(chord,direction=-1)$affinity)
  expect_equal(h(chord,direction=0)$brightness,h(chord,direction=-1)$brightness)
  chord = c(0,3,7,12)
  expect_equal(h(chord,direction=0)$affinity,h(chord,direction=+1)$affinity)
  expect_equal(h(chord,direction=0)$brightness,h(chord,direction=+1)$brightness)
  expect_equal(h(chord,direction=0)$affinity,h(chord,direction=-1)$affinity)
  expect_equal(h(chord,direction=0)$brightness,h(chord,direction=-1)$brightness)
})
test_that('the harmony of one pitch with non-zero explicit root behaves',{
  expect_equal(h(5)$affinity,h(5,direction = 0,root = 0)$affinity)
  expect_equal(h(5)$brightness,h(5,direction = 0,root = 0)$brightness)

  expect_equal(h(5)$affinity,h(10,direction = 0,root = 5)$affinity)
  expect_equal(h(5)$brightness,h(10,direction = 0,root = 5)$brightness)
})
test_that('brightness and affinity of the diatonic scales makes sense',{
  expect_true(!is.unsorted(dplyr::bind_rows(diatonic_scales())$brightness))
  expect_true(!is.unsorted(dplyr::bind_rows(diatonic_scales()[c(1,2,3,4)])$affinity))
  expect_true(!is.unsorted(dplyr::bind_rows(diatonic_scales()[c(7,6,5,4)])$affinity))
})

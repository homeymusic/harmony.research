test_that("up primary_frequency_ratios within the primary octave match expectations", {
  expect_equal(primary_frequency_ratios()$tonic.tone,
               c(1,16,9,6,5,4,7,3,8,5,16,15,2))
  expect_equal(primary_frequency_ratios()$tonic.ref,
               c(1,15,8,5,4,3,5,2,5,3, 9, 8,1))
})
test_that('tonic frequency ratios within and beyond the primary octave matches expectations', {
  expect_equal(tone(7)$tonic.tone,3)
  expect_equal(tone(7)$tonic.ref,2)

  expect_equal(tone(12)$tonic.tone,2)
  expect_equal(tone(12)$tonic.ref,1)

  expect_equal(tone(13)$tonic.tone,32)
  expect_equal(tone(13)$tonic.ref,15)

  expect_equal(tone(-1)$tonic.tone,15)
  expect_equal(tone(-1)$tonic.ref,16)

  expect_equal(tone(-13)$tonic.tone,15)
  expect_equal(tone(-13)$tonic.ref,32)

  expect_equal(tone(-25)$tonic.tone,15)
  expect_equal(tone(-25)$tonic.ref,64)

  expect_equal(tone(-37)$tonic.tone,15)
  expect_equal(tone(-37)$tonic.ref,128)
})
test_that("down primary_frequency_ratios within the primary octave match expectations", {
  expect_equal(primary_frequency_ratios()$octave.tone, c(1, 8, 9,3,5,2,5,3,4,5,8,15,1))
  expect_equal(primary_frequency_ratios()$octave.ref,  c(2,15,16,5,8,3,7,4,5,6,9,16,1))
})
test_that('octave frequency ratios within and beyond the primary octave matches expectations', {
  expect_equal(tone(7)$octave.tone,3)
  expect_equal(tone(7)$octave.ref,4)

  expect_equal(tone(12)$octave.tone,1)
  expect_equal(tone(12)$octave.ref,1)

  expect_equal(tone(13)$octave.tone,16)
  expect_equal(tone(13)$octave.ref,15)

  expect_equal(tone(25)$octave.tone,32)
  expect_equal(tone(25)$octave.ref,15)

  expect_equal(tone(37)$octave.tone,64)
  expect_equal(tone(37)$octave.ref,15)

  expect_equal(tone(-1)$octave.tone,15)
  expect_equal(tone(-1)$octave.ref,32)

  expect_equal(tone(-13)$octave.tone,15)
  expect_equal(tone(-13)$octave.ref,64)

  expect_equal(tone(-25)$octave.tone,15)
  expect_equal(tone(-25)$octave.ref,128)
})
test_that('octave complements match as expected',{
  expect_equal(tone(0)$tonic.tone,tone(12)$octave.tone)
  expect_equal(tone(7)$tonic.primes,tone(5)$octave.primes)
  expect_equal(tone(4)$tonic.primes,tone(8)$octave.primes)
  expect_equal(tone(12)$tonic.primes,tone(0)$octave.primes)
})
test_that('position in cents makes sense within primary octave',{
  expect_equal(abs(tone(0)$tonic.position) + abs(tone(0)$octave.position),1200)
  expect_equal(abs(tone(1)$tonic.position) + abs(tone(1)$octave.position),1200)
  expect_equal(abs(tone(5)$tonic.position) + abs(tone(5)$octave.position),1200)
  expect_equal(abs(tone(7)$tonic.position) + abs(tone(7)$octave.position),1200)
})

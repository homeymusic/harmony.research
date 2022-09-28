test_that("up primary_frequency_ratios within the primary octave match expectations", {
  expect_equal(primary_frequency_ratios()$tonic.pitch,
               c(1,16,9,6,5,4,7,3,8,5,16,15,2))
  expect_equal(primary_frequency_ratios()$tonic.ref,
               c(1,15,8,5,4,3,5,2,5,3, 9, 8,1))
})
test_that('tonic frequency ratios within and beyond the primary octave matches expectations', {
  expect_equal(pitch(7)$tonic.pitch,3)
  expect_equal(pitch(7)$tonic.ref,2)

  expect_equal(pitch(12)$tonic.pitch,2)
  expect_equal(pitch(12)$tonic.ref,1)

  expect_equal(pitch(13)$tonic.pitch,32)
  expect_equal(pitch(13)$tonic.ref,15)

  expect_equal(pitch(-1)$tonic.pitch,15)
  expect_equal(pitch(-1)$tonic.ref,16)

  expect_equal(pitch(-13)$tonic.pitch,15)
  expect_equal(pitch(-13)$tonic.ref,32)

  expect_equal(pitch(-25)$tonic.pitch,15)
  expect_equal(pitch(-25)$tonic.ref,64)

  expect_equal(pitch(-37)$tonic.pitch,15)
  expect_equal(pitch(-37)$tonic.ref,128)
})
test_that("down primary_frequency_ratios within the primary octave match expectations", {
  expect_equal(primary_frequency_ratios()$octave.pitch, c(1, 8, 9,3,5,2,5,3,4,5,8,15,1))
  expect_equal(primary_frequency_ratios()$octave.ref,  c(2,15,16,5,8,3,7,4,5,6,9,16,1))
})
test_that('octave frequency ratios within and beyond the primary octave matches expectations', {
  expect_equal(pitch(7)$octave.pitch,3)
  expect_equal(pitch(7)$octave.ref,4)

  expect_equal(pitch(12)$octave.pitch,1)
  expect_equal(pitch(12)$octave.ref,1)

  expect_equal(pitch(13)$octave.pitch,16)
  expect_equal(pitch(13)$octave.ref,15)

  expect_equal(pitch(25)$octave.pitch,32)
  expect_equal(pitch(25)$octave.ref,15)

  expect_equal(pitch(37)$octave.pitch,64)
  expect_equal(pitch(37)$octave.ref,15)

  expect_equal(pitch(-1)$octave.pitch,15)
  expect_equal(pitch(-1)$octave.ref,32)

  expect_equal(pitch(-13)$octave.pitch,15)
  expect_equal(pitch(-13)$octave.ref,64)

  expect_equal(pitch(-25)$octave.pitch,15)
  expect_equal(pitch(-25)$octave.ref,128)
})
test_that('octave complements match as expected',{
  expect_equal(pitch(0)$tonic.pitch,pitch(12)$octave.pitch)
  expect_equal(pitch(7)$tonic.primes,pitch(5)$octave.primes)
  expect_equal(pitch(4)$tonic.primes,pitch(8)$octave.primes)
  expect_equal(pitch(12)$tonic.primes,pitch(0)$octave.primes)
})
test_that('position in cents makes sense within primary octave',{
  expect_equal(abs(pitch(0)$tonic.position) + abs(pitch(0)$octave.position),1200)
  expect_equal(abs(pitch(1)$tonic.position) + abs(pitch(1)$octave.position),1200)
  expect_equal(abs(pitch(5)$tonic.position) + abs(pitch(5)$octave.position),1200)
  expect_equal(abs(pitch(7)$tonic.position) + abs(pitch(7)$octave.position),1200)
})
test_that('p is a synonym for pitch',{
  expect_equal(pitch(7),p(7))
})

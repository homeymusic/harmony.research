test_that("up pitch_class_ratios within the primary octave match expectations", {
  expect_equal(core_pitch_class_ratios()$tonic.num.hi,
               c(1,16,9,6,5,4,7,3,8,5,16,15,2))
  expect_equal(core_pitch_class_ratios()$tonic.den.lo,
               c(1,15,8,5,4,3,5,2,5,3, 9, 8,1))
})
test_that('tonic frequency ratios within and beyond the primary octave matches expectations', {
  expect_equal(pitch(7)$tonic.num.hi,3)
  expect_equal(pitch(7)$tonic.den.lo,2)

  expect_equal(pitch(12)$tonic.num.hi,2)
  expect_equal(pitch(12)$tonic.den.lo,1)

  expect_equal(pitch(13)$tonic.num.hi,32)
  expect_equal(pitch(13)$tonic.den.lo,15)

  expect_equal(pitch(-1)$tonic.num.hi,15)
  expect_equal(pitch(-1)$tonic.den.lo,16)

  expect_equal(pitch(-13)$tonic.num.hi,15)
  expect_equal(pitch(-13)$tonic.den.lo,32)

  expect_equal(pitch(-25)$tonic.num.hi,15)
  expect_equal(pitch(-25)$tonic.den.lo,64)

  expect_equal(pitch(-37)$tonic.num.hi,15)
  expect_equal(pitch(-37)$tonic.den.lo,128)
})
test_that("down pitch_class_ratios within the primary octave match expectations", {
  expect_equal(core_pitch_class_ratios()$octave.num.lo, c(1, 8, 9,3,5,2,5,3,4,5,8,15,1))
  expect_equal(core_pitch_class_ratios()$octave.den.hi,  c(2,15,16,5,8,3,7,4,5,6,9,16,1))
})
test_that('octave frequency ratios within and beyond the primary octave matches expectations', {
  expect_equal(pitch(7)$octave.num.lo,3)
  expect_equal(pitch(7)$octave.den.hi,4)

  expect_equal(pitch(12)$octave.num.lo,1)
  expect_equal(pitch(12)$octave.den.hi,1)

  expect_equal(pitch(13)$octave.num.lo,16)
  expect_equal(pitch(13)$octave.den.hi,15)

  expect_equal(pitch(25)$octave.num.lo,32)
  expect_equal(pitch(25)$octave.den.hi,15)

  expect_equal(pitch(37)$octave.num.lo,64)
  expect_equal(pitch(37)$octave.den.hi,15)

  expect_equal(pitch(-1)$octave.num.lo,15)
  expect_equal(pitch(-1)$octave.den.hi,32)

  expect_equal(pitch(-13)$octave.num.lo,15)
  expect_equal(pitch(-13)$octave.den.hi,64)

  expect_equal(pitch(-25)$octave.num.lo,15)
  expect_equal(pitch(-25)$octave.den.hi,128)
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
test_that('octave complements match as expected',{
  expect_equal(pitch(0)$tonic.num.hi,pitch(12)$octave.num.lo)
})
test_that('octave spanning pitches are expressed in lowest terms',{
  M10 = pitch(16)
  expect_equal(c(M10$tonic.num.hi,M10$tonic.den.lo),c(5,2))
})

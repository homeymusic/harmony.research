test_that("up frequency_ratios within the primary octave match expectations", {
  expect_equal(frequency_ratios()$probe.freq.asc,  c(1,16,9,6,5,4,7,3,8,5,16,15,2))
  expect_equal(frequency_ratios()$ref.freq.tonic,c(1,15,8,5,4,3,5,2,5,3, 9, 8,1))
})
test_that('up frequency_ratio within and beyond the primary octave matches expectations', {
  expect_equal(frequency_ratio(7,1)[['probe.freq']],3)
  expect_equal(frequency_ratio(7,1)[['ref.freq']],2)

  expect_equal(frequency_ratio(12,1)[['probe.freq']],2)
  expect_equal(frequency_ratio(12,1)[['ref.freq']],1)

  expect_equal(frequency_ratio(13,1)[['probe.freq']],32)
  expect_equal(frequency_ratio(13,1)[['ref.freq']],15)

  expect_equal(frequency_ratio(-1,1)[['probe.freq']],15)
  expect_equal(frequency_ratio(-1,1)[['ref.freq']],16)

  expect_equal(frequency_ratio(-13,1)[['probe.freq']],15)
  expect_equal(frequency_ratio(-13,1)[['ref.freq']],32)

  expect_equal(frequency_ratio(-25,1)[['probe.freq']],15)
  expect_equal(frequency_ratio(-25,1)[['ref.freq']],64)

  expect_equal(frequency_ratio(-37,1)[['probe.freq']],15)
  expect_equal(frequency_ratio(-37,1)[['ref.freq']],128)
})
test_that("down frequency_ratios within the primary octave match expectations", {
  expect_equal(frequency_ratios()$probe.freq.desc,  c(1, 8, 9,3,5,2,5,3,4,5,8,15,1))
  expect_equal(frequency_ratios()$ref.freq.octave,c(2,15,16,5,8,3,7,4,5,6,9,16,1))
})
test_that('down frequency_ratio within and beyond the primary octave matches expectations', {
  expect_equal(frequency_ratio(7,-1)[['probe.freq']],3)
  expect_equal(frequency_ratio(7,-1)[['ref.freq']],4)

  expect_equal(frequency_ratio(12,-1)[['probe.freq']],1)
  expect_equal(frequency_ratio(12,-1)[['ref.freq']],1)

  expect_equal(frequency_ratio(13,-1)[['probe.freq']],16)
  expect_equal(frequency_ratio(13,-1)[['ref.freq']],15)

  expect_equal(frequency_ratio(25,-1)[['probe.freq']],32)
  expect_equal(frequency_ratio(25,-1)[['ref.freq']],15)

  expect_equal(frequency_ratio(37,-1)[['probe.freq']],64)
  expect_equal(frequency_ratio(37,-1)[['ref.freq']],15)

  expect_equal(frequency_ratio(-1,-1)[['probe.freq']],15)
  expect_equal(frequency_ratio(-1,-1)[['ref.freq']],32)

  expect_equal(frequency_ratio(-13,-1)[['probe.freq']],15)
  expect_equal(frequency_ratio(-13,-1)[['ref.freq']],64)

  expect_equal(frequency_ratio(-25,-1)[['probe.freq']],15)
  expect_equal(frequency_ratio(-25,-1)[['ref.freq']],128)
})
test_that('up and down are the same',{
  expect_equal(frequency_ratio(0,1),frequency_ratio(12,-1))
  expect_equal(frequency_ratio(7,1)%>%sum,frequency_ratio(5,-1)%>%sum)
  expect_equal(frequency_ratio(4,1)%>%sum,frequency_ratio(8,-1)%>%sum)
  expect_equal(frequency_ratio(12,1)%>%sum,frequency_ratio(0,-1)%>%sum)
})

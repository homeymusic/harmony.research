test_that("melody is a tibble", {
  expect(tibble::is_tibble(melody(major_triad_progression())),
         'expected melody to be a tibble')
})

test_that("melody has m as synonym", {
  expect_equal(m(major_triad_progression()),melody(major_triad_progression()))
})

test_that('melody requires more than one row in the progressions', {
  expect_error(m(h(0)))
})

test_that('melody requires a tibble with min num of columns', {
  expect_error(m(rbind(tone(0),tone(1))))
})

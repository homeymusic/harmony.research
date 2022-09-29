test_that("melody is a tibble with appropriate number of rows", {
  p = major_triad_progression()
  m = melody(p)
  expect(tibble::is_tibble(m),
         'expected melody to be a tibble')
  expect_equal(nrow(m),length(p))
})

test_that("melody has m as synonym", {
  expect_equal(m(major_triad_progression()),melody(major_triad_progression()))
})

test_that('melody requires more than one row in the progressions', {
  expect_error(m(h(0)))
})

test_that('melody requires a tibble with min num of columns', {
  expect_error(m(rbind(pitch(0),pitch(1))))
})
test_that('if no reference harmony is given the first harmony in the progression is chosen',{
  p = major_triad_progression()
  m = melody(p)
  expect_equal(attr(m,'reference'),p[[1]])
})
test_that('if a reference harmony is given then it gets stored',{
  p = major_triad_progression()
  r = h(c(0))
  m = melody(p,r)
  expect_equal(attr(m,'reference'),r)
})
test_that('melody tibble includes the harmony columns',{
  p = major_triad_progression()
  r = h(c(0))
  m = melody(p,r)
  expect_equal(m$brightness[1],p[[1]]$brightness)
})


test_that("harmony throws an error is params are wrong", {
  expect_error(harmony())
  expect_error(h())
  expect_error(harmony('abc'))
  expect_error(h('abc'))
  expect_error(harmony(1,2))
  expect_error(h(1,2))
  expect(tibble::is_tibble(h(c(0,4,7))), 'expected harmony to be a tibble')
})

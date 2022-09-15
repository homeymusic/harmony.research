major_triad_root = major_triads()[["root"]]
major_triad_first_inversion = major_triads()[["1st inversion"]]
major_triad_second_inversion = major_triads()[["2nd inversion"]]

test_that('harmony throws an error if params are wrong', {
  expect_error(harmony())
  expect_error(h())
  expect_error(harmony('abc'))
  expect_error(h('abc'))
  expect_error(harmony(1,2))
  expect_error(h(1,2))
  expect_error(h(1,2,'a'))
  expect(tibble::is_tibble(h(c(0,4,7),+1,0,'major triad')),
         'expected harmony to be a tibble')
})
test_that('params are stored',{
  expect_equal(attr(major_triad_root,'chord'),c(0,4,7))
  expect_equal(major_triad_root$position,c(0,4,7)%>%mean)
  expect_equal(major_triad_root$direction,+1)
  expect_equal(major_triad_root$reference_tone,0)
  expect_equal(major_triad_root$name,'major triad')
})
test_that('intervallic names are informative',{
  expect_equal(major_triad_root$intervallic_name,"0\u0332:4:7\u21D1")
  expect_equal(major_triad_first_inversion$intervallic_name,"4:7:1\u03322\u0332\u21D3")
  expect_equal(major_triad_second_inversion$intervallic_name,"7:1\u03322\u0332:16\u21D3")
})

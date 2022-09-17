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
test_that('intervallic names are informative and maintain voice leading order',{
  expect_equal(major_triad_root$intervallic_name,"0\u0332:4:7\u21D1")
  expect_equal(major_triad_first_inversion$intervallic_name,"1\u03322\u0332:4:7\u21D3")
  expect_equal(major_triad_second_inversion$intervallic_name,"1\u03322\u0332:16:7\u21D1\u21D3")
})
test_that('harmony will default to up and guess the reference tone',{
  h = h(c(0,4,7))
  expect_equal(h$direction,+1)
  expect_equal(h$reference_tone,0)
  h = h(c(0,4,7),.direction=-1)
  expect_equal(h$direction,-1)
  expect_equal(h$reference_tone,7)
  h = h(c(0,4,7),.direction=-1,.reference_tone=4)
  expect_equal(h$direction,-1)
  expect_equal(h$reference_tone,4)
})
test_that("interval affinity behaves well",{
  purrr::pmap(intervals(),~expect_equal(h(..1)$affinity,..4,info=paste('..1',..1,'..3',..4)))
})
test_that("interval brightness behaves well",{
  purrr::pmap(intervals(),~expect_equal(h(..1)$brightness,..3))
})
test_that("exponent prime factors sum works as expected",{
  expect_equal(exponent_prime_factors_sum(1),0)
  expect_equal(exponent_prime_factors_sum(2),2)
  expect_equal(exponent_prime_factors_sum(6),5)
  expect_equal(exponent_prime_factors_sum(10),7)
})
test_that("dissonance measure matches expectations", {
  expected_up.dissonance = c(0,16,12,10,9,7,12,5,11,8,14,14,2)
  purrr::pmap(intervals(),~expect_equal(
    dissonance(..1)[1],
    expected_up.dissonance[..1+1],
    info=paste('position: ',..1,..2,'up num: ',ratios$up.numerator[..1+1],'up denominator: ',ratios$up.denominator[..1+1]))
  )
  expected_down.dissonance = c(2,14,14,8,11,5,12,7,9,10,12,16,0)
  intervals() %>% purrr::pmap(~expect_equal(
    dissonance(..1)[2],
    expected_down.dissonance[..1+1],
    info=paste(..1,..2,ratios$down.numerator[..1+1],ratios$down.denominator[..1+1]))
  )
})
test_that('upper bound of dissonance makes sense',{
  expect_equal(dissonance_upper_bound(),16)
})
test_that("rotation works", {
  angle = pi/4
  expect_equal(rotate(cbind(x=1,y=0),angle),cbind(0.5,0.5))
  expect_equal(rotate(cbind(x=0,y=1),angle),cbind(-0.5,0.5))
})
test_that('for 1 tone and no reference tone assume reference tone is zero',{
  expect_equal(h(6)$reference_tone,0)
})

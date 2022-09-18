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
  expect_equal(major_triad_root$root,0)
  expect_equal(major_triad_root$name,'major triad')
})
test_that('intervallic names are informative and maintain voice leading order',{
  expect_equal(major_triad_root$intervallic_name,"0\u0332:4:7\u21D1")
  expect_equal(major_triad_first_inversion$intervallic_name,"1\u03322\u0332:4:7\u21D3")
  expect_equal(major_triad_second_inversion$intervallic_name,"1\u03322\u0332:16:7\u21D3")
})
test_that('if implicit and explicit direction agree then do not flip it.',{
  expect_gt(major_triad_first_inversion$brightness,0)
})
test_that('implicit reference tones make sense',{
  expect_equal(major_triad_first_inversion$implicit_root,12)
  expect_equal(h(c(1,2,3))$implicit_root,1)
})
test_that('harmony will default to up and guess the reference tone',{
  h = h(c(0,4,7))
  expect_equal(h$direction,+1)
  expect_equal(h$root,0)
  h = h(c(0,4,7),direction=-1)
  expect_equal(h$direction,-1)
  expect_equal(h$root,7)
  h = h(c(0,4,7),direction=-1,root=4)
  expect_equal(h$direction,-1)
  expect_equal(h$root,4)
  h = h(c(0,4,7,12))
  expect_equal(h$direction,0)
  expect_equal(h$root,0)
  h = h(-c(0,4,7))
  expect_equal(h$direction,-1)
  expect_equal(h$root,0)
})
test_that("interval affinity behaves well",{
  purrr::pmap(intervals(),~expect_equal(h(..1,1)$affinity,..4,info=paste('interval:',..1,'affinity',..4)))
})
test_that("interval brightness behaves well",{
  purrr::pmap(intervals(),~expect_equal(h(..1,1)$brightness,..3))
})
test_that("exponent prime factors sum works as expected",{
  expect_equal(count_primes(1),1)
  expect_equal(count_primes(2),2)
  expect_equal(count_primes(6),5)
  expect_equal(count_primes(10),7)
})
test_that("dissonance measure matches expectations", {
  expected_up_primes = c(2,16,12,10,9,7,12,5,11,8,14,14,3)
  purrr::pmap(intervals(),~expect_equal(
    dissonance(..1)[1,1],
    expected_up_primes[..1+1],
    info=paste('position:',..1,..2,'probe.freq:',frequency_ratio(..1,1),'ref.freq:',frequency_ratio(..1,1))
  ))
  expected_down_primes = c(3,14,14,8,11,5,12,7,9,10,12,16,2)
  intervals() %>% purrr::pmap(~expect_equal(
    dissonance(..1)[1,2],
    expected_down_primes[..1+1],
    info=paste('position:',..1,..2,'probe.freq:',frequency_ratio(..1,1),'ref.freq',frequency_ratio(..1,1))
  ))
})
test_that('upper bound of dissonance makes sense',{
  expect_equal(max_dissonance(),16)
})
test_that("rotation works", {
  angle = pi/4
  expect_equal(rotate(cbind(x=1,y=0),angle),cbind(0.5,0.5))
  expect_equal(rotate(cbind(x=0,y=1),angle),cbind(-0.5,0.5))
})
test_that('for 1 tone and no reference tone assume reference tone is zero',{
  expect_equal(h(6)$root,0)
})
test_that('brightness and affinity are symmetrical with symmetrical chords',{
  expect_equal(h(c(0,4,7))$affinity,h(-c(0,4,7),-1)$affinity)
  expect_equal(h(c(0,4,7))$brightness,-h(-c(0,4,7),-1)$brightness)

  expect_equal(h(c(0,3,7))$affinity,h(-c(0,3,7),-1)$affinity)
  expect_equal(h(c(0,3,7))$brightness,-h(-c(0,3,7),-1)$brightness)
})
test_that('consonance is the L1 of affinity and brightness',{
  expect_equal(h(c(0,4,7))$consonance,
               abs(h(c(0,4,7))$brightness)+abs(h(c(0,4,7),)$affinity))

  expect_equal(h(c(0,4,7))$consonance,h(-c(0,4,7),-1)$consonance)

})
test_that('aural centering works as expected',{
  # do we detect the inversion when
  # the tonal center (0) happens to be the same as the aural center (0)?
  expect_equal(h(c(0+12,4,7))$implicit_direction,-1)
  # do we detect the inversion when
  # the tonal center (60) happens to be different tha the aural center (0)?
  expect_equal(h(c(0+12,4,7)+60)$implicit_direction,-1)
})

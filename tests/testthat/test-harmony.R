major_triad_root = major_triads()[["root"]]
major_triad_first_inversion = major_triads()[["1st inversion"]]
major_triad_second_inversion = major_triads()[["2nd inversion"]]

minor_triad_root = minor_triads()[["root"]]
minor_triad_first_inversion = minor_triads()[["1st inversion"]]
minor_triad_second_inversion = minor_triads()[["2nd inversion"]]

locrian = diatonic_scales()[['locrian']]

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
test_that('integer names are informative and maintain voice leading order',{
  # major triads
  expect_equal(major_triad_root$integer_name,"0\u0332:4:7\u21D1")
  expect_equal(major_triad_first_inversion$integer_name,"1\u03322\u0332:4:7\u21D3")
  expect_equal(major_triad_second_inversion$integer_name,"1\u03322\u0332:16:7\u21D3")

  # minor triads
  expect_equal(minor_triad_root$integer_name,"0\u0332:3:7\u21D1")
  expect_equal(minor_triad_first_inversion$integer_name,"1\u03322\u0332:3:7\u21D3")
  expect_equal(minor_triad_second_inversion$integer_name,"1\u03322\u0332:15:7\u21D3")
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
  expect_equal(h(0)$affinity,h(12)$affinity)
  purrr::pmap(intervals(),~expect_equal(h(..1)$affinity,..4,info=paste('interval:',..1,'affinity',..4)))
})
test_that("interval brightness behaves well",{
  purrr::pmap(intervals(),~expect_equal(h(..1)$brightness,..3))
})
test_that("exponent prime factors sum works as expected",{
  expect_equal(sum_primes(1),0)
  expect_equal(sum_primes(2),2)
  expect_equal(sum_primes(6),5)
  expect_equal(sum_primes(10),7)
})
test_that("dissonance measure matches expectations", {
  expected_up_primes = c(0,16,12,10,9,7,12,5,11,8,14,14,2)
  purrr::pmap(intervals(),~expect_equal(
    tonic_octave_dissonance(..1)[1,1],
    expected_up_primes[..1+1],
    info=paste('position:',..1,..2,'probe.freq:',frequency_ratio(..1,1),'ref.freq:',frequency_ratio(..1,1))
  ))
  expected_down_primes = c(2,14,14,8,11,5,12,7,9,10,12,16,0)
  intervals() %>% purrr::pmap(~expect_equal(
    tonic_octave_dissonance(..1)[1,2],
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
  # do we detect the inversion when tonal center (0) same as the aural center (0)?
  h = h(c(0,4,7))
  expect_equal(h$implicit_direction,1)
  expect_equal(attr(h,"aurally_centered_chord"),c(0,4,7))

  # do we detect the inversion when tonal center (12) happens to be 12?
  h = h(c(0+12,4,7))
  expect_equal(h$implicit_direction,-1)
  expect_equal(attr(h,"aurally_centered_chord"),c(12,4,7))

  # c major 2d inversion using midi notes and various levels of specificity
  h = h(c(12+0,12+4,7)+60)
  expect_equal(h$implicit_direction,1)
  expect_equal(h$direction,1)
  expect_equal(h$implicit_root,67)
  expect_equal(h$root,67)
  expect_equal(attr(h,"aurally_centered_chord"),c(5,9,0))
  h = h(c(12+0,12+4,7)+60,-1)
  expect_equal(h$implicit_direction,1)
  expect_equal(h$explicit_direction,-1)
  expect_equal(h$direction,-1)
  expect_equal(h$implicit_root,76)
  expect_equal(h$root,76)
  expect_equal(attr(h,"aurally_centered_chord"),c(8,12,3))
  h = h(c(12+0,12+4,7)+60,-1,76)
  expect_equal(h$implicit_direction,-1)
  expect_equal(h$explicit_direction,-1)
  expect_equal(h$direction,-1)
  expect_equal(h$implicit_root,76)
  expect_equal(h$explicit_root,76)
  expect_equal(h$root,76)
  expect_equal(attr(h,"aurally_centered_chord"),c(8,12,3))
})
test_that('implicit direction for minor triad and inversions makes sense',{
  expect_equal(h(c(0,3,7))$direction,1)
  expect_equal(h(c(0+12,3,7))$direction,-1)
  expect_equal(h(c(0+12,3+12,7))$direction,-1)
})
test_that('the major triad is perfectly bright. and the minor triad is a third', {
  expect_equal(major_triad_root$brightness,1)
  expect_equal(minor_triad_root$brightness,1/3,tolerance=0.00001)
  # symetrical triads are interesting as well
  expect_equal(h(c(0,4,7,12))$brightness,0.5)
  expect_equal(h(c(0,3,7,12))$brightness,0.0)
})
test_that('the similarities among major and minor triads under inversion are interesting',{
  expect_equal(major_triad_root$affinity,major_triad_first_inversion$affinity)
  expect_equal(major_triad_first_inversion$brightness,major_triad_second_inversion$brightness)

  expect_equal(minor_triad_root$affinity,minor_triad_first_inversion$affinity)
  expect_equal(minor_triad_first_inversion$brightness,minor_triad_second_inversion$brightness)
})
test_that('for solo tones that the integer name includes the tonic, octave and both arrows',{
  expect_equal(h(c(7))$integer_name,'0\u0332 7\u21D1\u21D3 1\u03322\u0332')
  expect_equal(locrian$integer_name,'0\u0332:1:3:5:6:8:10:1\u03322\u0332\u21D1\u21D3')
  expect_equal(h(c(0,-4,-7),direction=1,root=-7)$integer_name,'0:-4:-\u03327\u0332\u21D1')
})
test_that("tonic-octave symmetrical chords have identical consonance regardless of direction",{
  chord = c(0,4,7,12)
  expect_equal(h(chord,direction=0)$affinity,h(chord,direction=+1)$affinity)
  expect_equal(h(chord,direction=0)$brightness,h(chord,direction=+1)$brightness)
  expect_equal(h(chord,direction=0)$affinity,h(chord,direction=-1)$affinity)
  expect_equal(h(chord,direction=0)$brightness,h(chord,direction=-1)$brightness)
  chord = c(0,3,7,12)
  expect_equal(h(chord,direction=0)$affinity,h(chord,direction=+1)$affinity)
  expect_equal(h(chord,direction=0)$brightness,h(chord,direction=+1)$brightness)
  expect_equal(h(chord,direction=0)$affinity,h(chord,direction=-1)$affinity)
  expect_equal(h(chord,direction=0)$brightness,h(chord,direction=-1)$brightness)
})

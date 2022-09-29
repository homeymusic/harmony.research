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
  expect_equal(major_triad_root$integer_position,c(0,4,7)%>%mean)
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
test_that('guessed roots make sense',{
  expect_equal(major_triad_first_inversion$guessed_root,12)
  expect_equal(h(c(1,2,3))$guessed_root,1)
  expect_equal(h(c(12,4,7),root=4)$guessed_root,12)
})
test_that('harmony will default to up and guess the reference pitch',{
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

test_that('brightness and affinity are symmetrical with symmetrical chords',{
  expect_equal(h(c(0,4,7))$affinity,h(-c(0,4,7),-1)$affinity)
  expect_equal(h(c(0,4,7))$brightness,-h(-c(0,4,7),-1)$brightness)

  expect_equal(h(c(0,3,7))$affinity,h(-c(0,3,7),-1)$affinity)
  expect_equal(h(c(0,3,7))$brightness,-h(-c(0,3,7),-1)$brightness)
})

test_that('aural centering works as expected',{
  # do we detect the inversion when tonal center (0) same as the aural center (0)?
  h = h(c(0,4,7))
  expect_equal(h$guessed_direction,1)
  expect_equal(attr(h,"centered_chord"),c(0,4,7))

  # do we detect the inversion when tonal center (12) happens to be 12?
  h = h(c(0+12,4,7))
  expect_equal(h$guessed_direction,-1)
  expect_equal(attr(h,"centered_chord"),c(12,4,7))

  # c major 2d inversion using midi notes and various levels of specificity
  h = h(c(12+0,12+4,7)+60)
  expect_equal(h$guessed_direction,1)
  expect_equal(h$direction,1)
  expect_equal(h$guessed_root,67)
  expect_equal(h$root,67)
  expect_equal(attr(h,"centered_chord"),c(5,9,0))
  h = h(c(12+0,12+4,7)+60,-1)
  expect_equal(h$guessed_direction,1)
  expect_equal(h$explicit_direction,-1)
  expect_equal(h$direction,-1)
  expect_equal(h$guessed_root,76)
  expect_equal(h$root,76)
  expect_equal(attr(h,"centered_chord"),c(8,12,3))
  h = h(c(12+0,12+4,7)+60,-1,76)
  expect_equal(h$guessed_direction,-1)
  expect_equal(h$explicit_direction,-1)
  expect_equal(h$direction,-1)
  expect_equal(h$guessed_root,76)
  expect_equal(h$explicit_root,76)
  expect_equal(h$root,76)
  expect_equal(attr(h,"centered_chord"),c(8,12,3))
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
test_that('for solo pitches that the integer name includes the tonic, octave and both arrows',{
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

test_that('position from the tonic in cents makes sense',{
  expect_equal(h(c(0,4,7))$position,362.7562,tolerance=0.001)
  expect_equal(h(c(0,3,7))$position,339.1988,tolerance=0.001)
  expect_equal(h(c(4))$position,pitch(4)$tonic.position)
})
test_that('the harmony of one pitch with non-zero explicit root behaves',{
  expect_equal(h(5)$affinity,h(5,direction = 0,root = 0)$affinity)
  expect_equal(h(5)$brightness,h(5,direction = 0,root = 0)$brightness)

  expect_equal(h(5)$affinity,h(10,direction = 0,root = 5)$affinity)
  expect_equal(h(5)$brightness,h(10,direction = 0,root = 5)$brightness)
})
test_that('for chords of length 1 the direction must be 0',{
  expect_error(h(7,-1))
})
test_that('brightness and affinity of the diatonic scales makes sense',{
  expect_true(!is.unsorted(dplyr::bind_rows(diatonic_scales())$brightness))
  expect_true(!is.unsorted(dplyr::bind_rows(diatonic_scales()[c(1,2,3,4)])$affinity))
  expect_true(!is.unsorted(dplyr::bind_rows(diatonic_scales()[c(7,6,5,4)])$affinity))
})

test_that('harmony throws an error if params are wrong', {
  expect_error(harmony())
  expect_error(h())
  expect_error(harmony('abc'))
  expect_error(h('abc'))
  expect_error(harmony(1,2))
  expect_error(h(1,2))
  expect_error(h(1,2,'a'))
  expect(tibble::is_tibble(h(c(0,4,7),0,0,'major triad')),
         'expected harmony to be a tibble')
})
test_that('params are stored',{
  expect_equal(attr(major_triad_root,'chord'),c(0,4,7))
  expect_equal(major_triad_root$integer,c(0,4,7)%>%mean)
  expect_equal(major_triad_root$observation_point,0)
  expect_equal(major_triad_root$root,0)
  expect_equal(major_triad_root$name,'Major Triad Root')
})
test_that('integer names are informative and maintain voice leading order',{
  # major triads
  expect_equal(major_triad_root$integer_name,"0\u0332:4:7↑")
  expect_equal(major_triad_first_inversion$integer_name,"0:3:8̲↓")
  expect_equal(major_triad_second_inversion$integer_name,"0:5:9̲↓")

  # minor triads
  expect_equal(minor_triad_root$integer_name,"0\u0332:3:7↑")
  expect_equal(minor_triad_first_inversion$integer_name,"0:4:9̲↓")
  expect_equal(minor_triad_second_inversion$integer_name,"0:5:8̲↓")
})
test_that('if implicit and explicit observation_point agree then do not flip it.',{
  expect_gt(major_triad_first_inversion$primes.brightness,0)
})
test_that('guessed roots make sense',{
  expect_equal(major_triad_first_inversion$guessed_root,8)
  expect_equal(h(c(1,2,3))$guessed_root,1)
  expect_equal(h(c(12,4,7),root=4)$guessed_root,12)
})
test_that('harmony will default to up and guess the reference pitch',{
  h = h(c(0,4,7))
  expect_equal(h$observation_point,0)
  expect_equal(h$root,0)
  h = h(c(0,4,7),observation_point=12)
  expect_equal(h$observation_point,12)
  expect_equal(h$root,7)
  h = h(c(0,4,7),observation_point=12,root=4)
  expect_equal(h$observation_point,12)
  expect_equal(h$root,4)
  h = h(c(0,4,7,12))
  expect_equal(h$observation_point,NA)
  expect_equal(h$root,0)
  h = h(-c(0,4,7))
  expect_equal(h$observation_point,12)
  expect_equal(h$root,0)
})

test_that('aural centering works as expected',{
  # do we detect the inversion when tonal center (0) same as the aural center (0)?
  h = h(c(0,4,7))
  expect_equal(h$guessed_observation_point,0)
  expect_equal(attr(h,"centered_chord"),c(0,4,7))

  # do we detect the inversion when tonal center (12) happens to be 12?
  h = h(c(0+12,4,7))
  expect_equal(h$guessed_observation_point,12)
  expect_equal(attr(h,"centered_chord"),c(12,4,7))

  # c major 2d inversion using midi notes and various levels of specificity
  h = h(c(12+0,12+4,7)+60)
  expect_equal(h$guessed_observation_point,0)
  expect_equal(h$observation_point,0)
  expect_equal(h$guessed_root,67)
  expect_equal(h$root,67)
  expect_equal(attr(h,"centered_chord"),c(5,9,0))
  h = h(c(12+0,12+4,7)+60,12)
  expect_equal(h$guessed_observation_point,0)
  expect_equal(h$explicit_observation_point,12)
  expect_equal(h$observation_point,12)
  expect_equal(h$guessed_root,76)
  expect_equal(h$root,76)
  expect_equal(attr(h,"centered_chord"),c(8,12,3))
  h = h(c(12+0,12+4,7)+60,12,76)
  expect_equal(h$guessed_observation_point,12)
  expect_equal(h$explicit_observation_point,12)
  expect_equal(h$observation_point,12)
  expect_equal(h$guessed_root,76)
  expect_equal(h$explicit_root,76)
  expect_equal(h$root,76)
  expect_equal(attr(h,"centered_chord"),c(8,12,3))
})
test_that('implicit observation_point for minor triad and inversions makes sense',{
  expect_equal(h(c(0,3,7))$observation_point,0)
  expect_equal(h(c(0+12,3,7))$observation_point,12)
  expect_equal(h(c(0+12,3+12,7))$observation_point,12)
})
test_that('for solo pitches that the integer name includes the tonic, octave and both arrows',{
  expect_equal(h(c(0))$integer_name,'0\u0332↑↓ 1\u03322\u0332')
  expect_equal(h(c(12))$integer_name,'0̲ 1̲2̲↑↓')
  expect_equal(h(c(7))$integer_name,'0\u0332 7↑↓ 1\u03322\u0332')
  expect_equal(locrian$integer_name,'0̲:1:3:5:6:8:10:1\u03322\u0332↑↓')
  expect_equal(h(c(0,-4,-7),observation_point=0,root=-7)$integer_name,'0:-4:-\u03327\u0332↑')

  expect_equal(h(c(0+60),root=60)$integer_name,'6\u03320\u0332↑↓ 7\u03322\u0332')
  expect_equal(h(c(12+60),root=60)$integer_name,"6̲0̲ 72↑↓")
  expect_equal(h(c(7+60),root=60)$integer_name,'6\u03320\u0332 67↑↓ 7\u03322\u0332')
  expect_equal(h(attr(locrian,'chord')+60)$integer_name,"6̲0̲:61:63:65:66:68:70:72↑↓")
  expect_equal(h(c(0,-4,-7)+60,observation_point=0,root=-7+60)$integer_name,'60:56:5\u03323\u0332↑')
})
test_that('position from the tonic in cents makes sense',{
  expect_equal(h(c(0,4,7))$cents,362.7562,tolerance=0.001)
  expect_equal(h(c(0,3,7))$cents,339.1988,tolerance=0.001)
  expect_equal(h(c(4))$cents,pitch(4)$cents)
})
test_that('for chords of length 1 the observation_point must be 0',{
  expect_error(h(7,12))
})
test_that('brightness and affinity are symmetrical with symmetrical chords',{
  expect_equal(h(c(0,4,7))$primes.affinity,h(-c(0,4,7),12)$primes.affinity)
  expect_equal(h(c(0,4,7))$primes.brightness,-h(-c(0,4,7),12)$primes.brightness)

  expect_equal(h(c(0,3,7))$primes.affinity,h(-c(0,3,7),12)$primes.affinity)
  expect_equal(h(c(0,3,7))$primes.brightness,-h(-c(0,3,7),12)$primes.brightness)
})
test_that('the major triad is perfectly bright. and the minor triad is a third', {
  expect_equal(major_triad_root$primes.brightness,1)
  expect_equal(minor_triad_root$primes.brightness,1/3,tolerance=0.00001)
  # symmetrical triads are interesting as well
  expect_equal(h(c(0,4,7,12))$primes.brightness,0.5)
  expect_equal(h(c(0,3,7,12))$primes.brightness,0.0)
})
test_that('the similarities among major and minor triads under inversion are interesting',{
  expect_equal(major_triad_root$primes.affinity,major_triad_first_inversion$primes.affinity)

  expect_equal(minor_triad_root$primes.affinity,minor_triad_first_inversion$primes.affinity)
})
test_that("tonic-octave symmetrical chords have identical consonance regardless of observation_point",{
  chord = c(0,4,7,12)
  expect_equal(h(chord,observation_point=0)$primes.affinity,h(chord,observation_point=0)$primes.affinity)
  expect_equal(h(chord,observation_point=0)$primes.brightness,h(chord,observation_point=0)$primes.brightness)
  expect_equal(h(chord,observation_point=0)$primes.affinity,h(chord,observation_point=12)$primes.affinity)
  expect_equal(h(chord,observation_point=0)$primes.brightness,h(chord,observation_point=12)$primes.brightness)
  chord = c(0,3,7,12)
  expect_equal(h(chord,observation_point=0)$primes.affinity,h(chord,observation_point=0)$primes.affinity)
  expect_equal(h(chord,observation_point=0)$primes.brightness,h(chord,observation_point=0)$primes.brightness)
  expect_equal(h(chord,observation_point=0)$primes.affinity,h(chord,observation_point=12)$primes.affinity)
  expect_equal(h(chord,observation_point=0)$primes.brightness,h(chord,observation_point=12)$primes.brightness)
})
test_that('the harmony of one pitch with non-zero explicit root behaves',{
  expect_equal(h(5)$primes.affinity,h(5,observation_point = NA,root = 0)$primes.affinity)
  expect_equal(h(5)$primes.brightness,h(5,observation_point = NA,root = 0)$primes.brightness)

  expect_equal(h(5)$primes.affinity,h(10,observation_point = NA,root = 5)$primes.affinity)
  expect_equal(h(5)$primes.brightness,h(10,observation_point = NA,root = 5)$primes.brightness)
})
test_that('brightness and affinity of the diatonic scales makes sense',{
  expect_true(!is.unsorted(dplyr::bind_rows(diatonic_scales())$primes.brightness))
  expect_true(!is.unsorted(dplyr::bind_rows(diatonic_scales()[c(1,2,3,4)])$primes.affinity))
  expect_true(!is.unsorted(dplyr::bind_rows(diatonic_scales()[c(7,6,5,4)])$primes.affinity))
})
test_that('harmony guesses that a chord containing root and root + 12 has observation_point = 0',{
  expect_equal(locrian$observation_point,NA)
  expect_equal(h(attr(locrian,'chord')+60)$observation_point,NA)
})
test_that('default consonance metric works as expected',{
  minor_triad_stolzenburg2015 = h(c(0,3,7))
  # stolzenburg is default
  expect_equal(minor_triad_stolzenburg2015$affinity,minor_triad_stolzenburg2015$stolzenburg2015.affinity)
  expect_equal(minor_triad_stolzenburg2015$brightness,minor_triad_stolzenburg2015$stolzenburg2015.brightness)
  expect_equal(minor_triad_stolzenburg2015$integer_name,"0̲:3:7↑")
  expect_equal(minor_triad_stolzenburg2015$brightness,-0.8684828,tolerance = 0.001)
  expect_equal(minor_triad_stolzenburg2015$affinity,1.453445,tolerance = 0.001)
  # can switch default to some other consonance metric and it works
  minor_triad_primes = h(c(0,3,7),default_consonance_metric = 'primes')
  expect_equal(minor_triad_primes$affinity,minor_triad_primes$primes.affinity)
  expect_equal(minor_triad_primes$brightness,minor_triad_primes$primes.brightness)
  expect_equal(minor_triad_primes$brightness,minor_triad_primes$primes.brightness)
})
test_that('default name works',{
  minor_triad = h(c(0,3,7))
  expect_equal(minor_triad$name,NA)
  explicit_name = 'minor triad'
  minor_triad = h(c(0,3,7),name=explicit_name)
  expect_equal(minor_triad$name,explicit_name)
})
test_that('labels make sense',{
  expect_equal(h(0)$label,"0̲↑↓ 1̲2̲")
  expect_equal(h(0,name='tonic')$label,"0̲↑↓ 1̲2̲\ntonic")
})

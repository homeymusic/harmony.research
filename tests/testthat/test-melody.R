p = major_triad_progression()
r = p[[1]]
m = melody(p,r)

test_that("melody is a tibble with appropriate number of rows", {
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
  m = melody(p)
  expect_equal(attr(m,'reference'),p[[1]])
})
test_that('if a reference harmony is given then it gets stored',{
  expect_equal(attr(m,'reference'),r)
})
test_that('melody tibble includes the harmony columns',{
  expect_equal(m$.brightness[1],p[[1]]$brightness)
})
test_that('integer name looks good for progressions',{
  expect_equal(m$integer_name[3],'5̲:9:12⇑ ⇒ 7̲:11:14⇑ (0̲:4:7⇑)')
})
test_that('progression fundamentals make sense',{
  i=ionian_tonic_chords()
  m=m(i)
  expect_equal(m$.brightness[4],i[[4]]$brightness)
  expect_equal(m$.affinity[4],i[[4]]$affinity)
  expect_equal(m$.name[4],i[[4]]$name)
  expect_equal(max(dplyr::filter(m,.brightness==1)$potential_energy),
               m$potential_energy[[5]])
  expect_equal(max(m$kinetic_energy),
               m$kinetic_energy[[5]])
})
test_that('energy of chord witxh itself is zero',{
  expect_equal(potential_energy(p,r)[1],0)
  expect_equal(kinetic_energy(p,r)[1],0)
})

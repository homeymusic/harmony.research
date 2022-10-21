plot_affinity_brightness_up_down <- function(combos,combos_name) {
  chords_up   = combos %>% purrr::map(~h(.x,observation_point=0))
  chords_down = combos %>% purrr::map(~h(.x,observation_point=12))
  chords = dplyr::bind_rows(dplyr::bind_rows(chords_up),dplyr::bind_rows(chords_down))
  plot_affinity_brightness(chords,combos_name)
}
plot_affinity_brightness <- function(chords,chords_name) {
  dimensions = 'Affinity-Brightness'
  title = paste('Periodicity:',chords_name,dimensions)
  p=harmony_plot(chords,c('stolzenburg2015.brightness','stolzenburg2015.affinity'),title=title)
  save_harmony_plots(p)
  expect_true(!is.null(p))
  title = paste('Primes:',chords_name,dimensions)
  p=harmony_plot(chords,c('primes.brightness','primes.affinity'),title=title)
  save_harmony_plots(p)
  expect_true(!is.null(p))
}
plot_tonic_octave_dissonance <- function(chords,chords_name) {
  dimensions = 'Tonic-Octave Dissonance'
  title = paste('Periodicity:',chords_name,dimensions)
  chords = dplyr::bind_rows(0:12 %>% purrr::map(~h(.x,observation_point = NA)))
  p=harmony_plot(chords,c('stolzenburg2015.octave.dissonance','stolzenburg2015.tonic.dissonance'),title=title)
  save_harmony_plots(p)
  expect_true(!is.null(p))
}
test_that("dissonance plot of core pitches", {
  chords = dplyr::bind_rows(0:12 %>% purrr::map(~h(.x,observation_point = NA)))
  plot_tonic_octave_dissonance(chords,'Pitches')
})
test_that('plot of tonic-octave dissonance makes sense',{
  chords = dplyr::bind_rows(0:12 %>% purrr::map(~h(.x,observation_point = NA)))
  plot_affinity_brightness(chords,'Pitches')
})
test_that("plot all dyads", {
  combos  = utils::combn(1:12,1,function(x){c(0,x)} ,simplify=FALSE)
  plot_affinity_brightness_up_down(combos,'Dyads')
})
test_that("plot all triads", {
  combos  = utils::combn(1:11,2,function(x){c(0,x)} ,simplify=FALSE)
  plot_affinity_brightness_up_down(combos,'Triads')
})
test_that("plot all tetrads", {
  combos  = utils::combn(1:11,3,function(x){c(0,x)} ,simplify=FALSE)
  plot_affinity_brightness_up_down(combos,'Tetrads')
})
test_that("plot triads symmetrical", {
  combos             = utils::combn(1:11,2,function(x){c(0,x,12)} ,simplify=FALSE)
  chords_symmetrical = dplyr::bind_rows(combos %>% purrr::map(~h(.x,observation_point=NA)))
  plot_affinity_brightness(chords_symmetrical,'Triads Symmetrical')
})
test_that("plot tetrads symmetrical", {
  combos             = utils::combn(1:11,3,function(x){c(0,x,12)} ,simplify=FALSE)
  chords_symmetrical = dplyr::bind_rows(combos %>% purrr::map(~h(.x,observation_point=NA)))
  plot_affinity_brightness(chords_symmetrical,'Tetrads Symmetrical')
})
test_that("plot major and minor triads", {
  combos  = list(
    c(0,4,7), c(0,3,8), c(0,5,9), # major
    c(0,3,7), c(0,4,9), c(0,5,8)) # minor
  plot_affinity_brightness_up_down(combos,'Major and Minor Triads')
})
test_that("Major 1st Inversion", {
  chords = dplyr::bind_rows(
    h(c(0,4,7),observation_point=0),
    h(c(0,3,8),observation_point=12),
    h(c(0,3,8),observation_point=0),
    h(c(0,0)  ,observation_point=0),
    h(c(0,3)  ,observation_point=0),
    h(c(0,8)  ,observation_point=0),
    h(c(0,5)  ,observation_point=0))
  plot_affinity_brightness(chords,'Major 1st Inversion')
})
test_that("tonic octave pitch ratio space is interesting", {
  midi_notes = 0:128
  middle_c = 60
  pitches = dplyr::bind_rows((midi_notes - middle_c) %>% purrr::map(p))
  tonic_ratio = pitches$tonic.num.hi / pitches$tonic.den.lo
  octave_ratio = pitches$octave.num.lo / pitches$octave.den.hi
  plot(tonic_ratio,octave_ratio)
  text(tonic_ratio,octave_ratio,midi_notes,pos=1)
  expect_true(TRUE)
})
test_that('diatonic modes look good',{
  chords = dplyr::bind_rows(diatonic_scales())
  plot_affinity_brightness(chords,'Diatonic Scales')
})

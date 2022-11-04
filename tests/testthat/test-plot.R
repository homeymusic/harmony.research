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
  title = paste('Roughness:',chords_name,dimensions)
  p=harmony_plot(chords,c('hutchinson1978.brightness','hutchinson1978.affinity'),title=title)
  save_harmony_plots(p)
  expect_true(!is.null(p))
}
plot_tonic_octave_dissonance <- function(chords,chords_name) {
  dimensions = 'Tonic-Octave Dissonance'
  title = paste('Periodicity:',chords_name,dimensions)
  p=harmony_plot(core_pitches(),c('stolzenburg2015.octave.dissonance','stolzenburg2015.tonic.dissonance'),title=title)
  save_harmony_plots(p)
  expect_true(!is.null(p))
}
test_that("dissonance plot of core pitches", {
  plot_tonic_octave_dissonance(core_pitches(),'Pitches')
})
test_that('plot of affinity brighness of core pitches makes sense',{
  plot_affinity_brightness(core_pitches(),'Pitches')
})
test_that("plot all dyads", {
  combos  = utils::combn(1:12,1,function(x){c(0,x)} ,simplify=FALSE)
  plot_affinity_brightness_up_down(combos,'Dyads')
})
test_that("plot all tonic triads", {
  combos  = utils::combn(1:11,2,function(x){c(0,x)} ,simplify=FALSE)
  plot_affinity_brightness_up_down(combos,'Tonic Triads')
})
# test_that("plot consonant floating triads", {
#   up_chords   = dplyr::bind_rows(utils::combn(0:11,3,function(x){h(x,observation_point=0,root=0)}  ,simplify=FALSE))
#   down_chords = dplyr::bind_rows(utils::combn(0:11,3,function(x){h(x,observation_point=12,root=0)} ,simplify=FALSE))
#   plot_affinity_brightness(dplyr::bind_rows(up_chords,down_chords) %>%
#                              dplyr::filter(affinity>=0.0),'Consonant Floating Triads')
# })
test_that("plot major and minor triads", {
  plot_affinity_brightness(major_minor_triads(),'Major and Minor Triads')
})
test_that("map the augmented triad hood", {
  combos = list(
    c(0,4,8),
    # 1st voice
    c(0-1,4,8),
    c(0+1,4,8),
    # 2nd voice
    c(0,4-1,8),
    c(0,4+1,8),
    # 3rd voice
    c(0,4,8-1),
    c(0,4,8+1)
  )
  plot_affinity_brightness_up_down(combos,'Neighborhood: Augmented Triad')
})
test_that("map the diminished seventh hood", {
  combos = list(
    c(0,3,6,9),
    # 1st voice
    c(0+1,3,6,9),
    c(0-1,3,6,9),
    # 2nd voice
    c(0,3+1,6,9),
    c(0,3-1,6,9),
    # 3rd voice
    c(0,3,6+1,9),
    c(0,3,6-1,9),
    # 4th voice
    c(0,3,6,9+1),
    c(0,3,6,9-1)
  )
  plot_affinity_brightness_up_down(combos,'Neighborhood: Diminished Seventh')
})
test_that("map the major triad hood", {
  combos = list(
    c(0,4,7),
    # 1st voice
    c(0+1,4,7),
    c(0-1,4,7),
    # 2nd voice
    c(0,4+1,7),
    c(0,4-1,7),
    # 3rd voice
    c(0,4,7+1),
    c(0,4,7-1)
  )
  plot_affinity_brightness_up_down(combos,'Neighborhood: Major Triad')
})
test_that("map the minor triad hood", {
  combos = list(
    c(0,3,7),
    # 1st voice
    c(0+1,3,7),
    c(0-1,3,7),
    # 2nd voice
    c(0,3+1,7),
    c(0,3-1,7),
    # 3rd voice
    c(0,3,7+1),
    c(0,3,7-1)
  )
  plot_affinity_brightness_up_down(combos,'Neighborhood: Minor Triad')
})
test_that("plot all major and minor triads plus one", {
  combos_major  = c(utils::combn(c(1:11)[c(-4,-7)],1,function(x){c(0,4,7,x) %>% sort} ,simplify=FALSE),
                    utils::combn(c(1:11)[c(-4,-7)],1,function(x){-c(0,4,7,x) %>% sort} ,simplify=FALSE))
  combos_minor  = c(utils::combn(c(1:11)[c(-3,-7)],1,function(x){c(0,3,7,x) %>% sort} ,simplify=FALSE),
                    utils::combn(c(1:11)[c(-3,-7)],1,function(x){-c(0,3,7,x) %>% sort} ,simplify=FALSE))
  combos = c(combos_major,combos_minor)
  plot_affinity_brightness_up_down(combos,'Tetrads: Major and Minor Triads Plus One')
})

test_that("plot triads in harmonic dualism", {
  plot_affinity_brightness(major_phrygian_triads(),'Harmonic Dualism? Major and Phrygian Triads')
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
test_that("plot all seventh chords", {
  plot_affinity_brightness(dplyr::bind_rows(seventh_chords()),'Seventh Chords')
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

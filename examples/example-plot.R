plot_tonic_octave_dissonance <- function() {
  dimensions = 'Tonic-Octave Dissonance'
  title = paste('Periodicity:','Pitches',dimensions)
  p=harmony_plot(core_pitches(),c('stolzenburg2015.octave.dissonance','stolzenburg2015.tonic.dissonance'),title=title)
  save_harmony_plots(p)
}

test_that("plot all tonic triads", {
  combos  = utils::combn(1:11,2,function(x){c(0,x)} ,simplify=FALSE)
  chords = chord_combinations(combos)
  plot_affinity_brightness(chords,'Tonic Triads')
})
test_that("plot consonant floating triads", {
  up_chords   = dplyr::bind_rows(utils::combn(0:11,3,function(x){h(x,observation_point=0,root=0)}  ,simplify=FALSE))
  down_chords = dplyr::bind_rows(utils::combn(0:11,3,function(x){h(x,observation_point=12,root=0)} ,simplify=FALSE))
  plot_affinity_brightness(dplyr::bind_rows(up_chords,down_chords) %>%
                             dplyr::filter(affinity>=0.0),'Consonant Floating Triads')
})
test_that("tonic octave pitch ratio space is interesting", {
  midi_notes = 0:128
  middle_c = 60
  pitches = dplyr::bind_rows((midi_notes - middle_c) %>% purrr::map(p))
  tonic_ratio = pitches$tonic.num.hi / pitches$tonic.den.lo
  octave_ratio = pitches$octave.num.lo / pitches$octave.den.hi
  plot(tonic_ratio,octave_ratio)
  text(tonic_ratio,octave_ratio,midi_notes,pos=1)
})
test_that("plot all seventh chords", {
  plot_affinity_brightness(dplyr::bind_rows(seventh_chords()),'Seventh Chords')
})
test_that("plot all dyads", {
  combos  = utils::combn(1:12,1,function(x){c(0,x)} ,simplify=FALSE)
  chords = chord_combinations(combos)
  plot_affinity_brightness(chords,'Dyads')
})
test_that("plot all major and minor triads plus one", {
  combos_major  = c(utils::combn(c(1:11)[c(-4,-7)],1,function(x){c(0,4,7,x) %>% sort} ,simplify=FALSE),
                    utils::combn(c(1:11)[c(-4,-7)],1,function(x){-c(0,4,7,x) %>% sort} ,simplify=FALSE))
  combos_minor  = c(utils::combn(c(1:11)[c(-3,-7)],1,function(x){c(0,3,7,x) %>% sort} ,simplify=FALSE),
                    utils::combn(c(1:11)[c(-3,-7)],1,function(x){-c(0,3,7,x) %>% sort} ,simplify=FALSE))
  chords = chord_combinations(c(combos_major,combos_minor))
  plot_affinity_brightness(chords,'Tetrads: Major and Minor Triads Plus One')
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
  chords = chord_combinations(combos)
  plot_affinity_brightness(chords,'Neighborhood: Diminished Seventh')
})

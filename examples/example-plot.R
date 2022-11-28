plot_tonic_octave_dissonance <- function() {
  dimensions = 'Tonic-Octave Dissonance'
  title = paste('Periodicity:','Pitches',dimensions)
  p=harmony_plot(core_pitches(),c('stolzenburg2015.octave.dissonance','stolzenburg2015.tonic.dissonance'),title=title)
  save_harmony_plots(p)
}
test_that('Triad Variations Ionian and Phrygian',{
  triad_variations = list(
    h( c(0,4,7)%>%sort,root=0,observation_point=TONIC),
    h(-c(0,4,7)%>%sort,root=0,observation_point=OCTAVE,midi_reference=60+4),
    h( c(0,5,9)%>%sort,root=0,observation_point=TONIC),
    h(-c(0,5,9)%>%sort,root=0,observation_point=OCTAVE,midi_reference=60+4),
    h( c(0,3,8)%>%sort,root=0,observation_point=TONIC),
    h(-c(0,3,8)%>%sort,root=0,observation_point=OCTAVE,midi_reference=60+4)
  )
  plot_affinity_brightness(dplyr::bind_rows(triad_variations),
                           'Triad Variations Ionian and Phrygian')
})
test_that('Triad Variations Aeolian and Mixolydian',{
  triad_variations = list(
    h( c(0,3,7)%>%sort,root=0,observation_point=TONIC ,midi_reference=60-3),
    h(-c(0,3,7)%>%sort,root=0,observation_point=OCTAVE,midi_reference=60+7),
    h( c(0,5,8)%>%sort,root=0,observation_point=TONIC ,midi_reference=60-3),
    h(-c(0,5,8)%>%sort,root=0,observation_point=OCTAVE,midi_reference=60+7),
    h( c(0,4,9)%>%sort,root=0,observation_point=TONIC ,midi_reference=60-3),
    h(-c(0,4,9)%>%sort,root=0,observation_point=OCTAVE,midi_reference=60+7)
  )
  plot_affinity_brightness(dplyr::bind_rows(triad_variations),
                           'Triad Variations Aeolian and Mixolydian')
})
test_that('Triad Variations Locrian and Lydian',{
  triad_variations = list(
    h( c(0,4,9)%>%sort,root=0,observation_point=TONIC ,midi_reference=60+5),
    h(-c(0,4,9)%>%sort,root=0,observation_point=OCTAVE,midi_reference=60-1),
    h( c(0,5,8)%>%sort,root=0,observation_point=TONIC ,midi_reference=60+5),
    h(-c(0,5,8)%>%sort,root=0,observation_point=OCTAVE,midi_reference=60-1),
    h( c(0,6,11)%>%sort,root=0,observation_point=TONIC ,midi_reference=60+5),
    h(-c(0,6,11)%>%sort,root=0,observation_point=OCTAVE,midi_reference=60-1)
  )
  plot_affinity_brightness(dplyr::bind_rows(triad_variations),
                           'Triad Variations Locrian and Lydian')
})
test_that('tonnetz plots work as expected', {
  time = 1
  x = c(0L,4L,7L)
  root = 7
  root_index = match(root,x)
  t <- tibble::tibble(
    time   = rep(1L,length(x)),
    pitch  = x,
    P5     = c(0L,0L,1L),
    M3     = c(0L,1L,0L)
  )
  pair_names = rep(NULL,length(x))
  pair_name_index = 1
  for (i in seq_along(x)) {
    pairs = -10000-seq(1:length(x))
    pairs[root_index] = time
    pairs[i] = time
    pair_names[pair_name_index] <- paste0('pair',pair_name_index)
    column_name = pair_names[pair_name_index]
    t <- tibble::add_column(t,
                            {{column_name}} := pairs
    )
    pair_name_index = pair_name_index + 1
  }
  p = t %>% ggplot2::ggplot(ggplot2::aes(P5,M3)) + ggplot2::geom_point()
  for (i in seq_along(pair_names)) {
    pair_name = pair_names[i]
    p = p + ggplot2::geom_line(ggplot2::aes(group=.data[[pair_name]]))
  }
  p
  expect_true(TRUE)
})
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
test_that("plot major and minor combinations", {
  chords = chord_combinations(list(c(0,4,7),c(0,3,7),c(0,4,8)))
  plot_affinity_brightness(chords,'Combinations: Major, Minor and Augmented Triads')
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
test_that("Cohn figure 2.1 map the major triad hood", {
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
  plot_affinity_brightness(chord_combinations(combos),'Neighborhood: Major Triad')
})
test_that("Cohn figure 2.1 map the minor triad hood", {
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
  plot_affinity_brightness(chord_combinations(combos),'Neighborhood: Minor Triad')
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
  plot_affinity_brightness(chord_combinations(combos),'Neighborhood: Augmented Triad')
})
###############
#
# Rehding start
#
test_that('Rehding Dualistic Forms figures 7.4 and 7.19 Triad of Triads',{
  triad_of_triads = list(
    h(c(-7,-4,0),root= 0,observation_point=OCTAVE,name='Expo 1',
      midi_reference=60+4),
    h(c(-4,0,3),root= 0,observation_point=OCTAVE,name='Expo 2',
      midi_reference=60+4),
    h(c(0,3,7),root= 0,observation_point=TONIC,name='Expo 3',
      midi_reference=60+4),
    h(c(0,3,7),root= 0,observation_point=TONIC,name='Coda 1',
      midi_reference=60-3),
    h(c(-4,0,3),root= 0,observation_point=TONIC,name='Coda 2',
      midi_reference=60-3),
    h(c(-7,-4,0),root= 0,observation_point=OCTAVE,name='Coda 3',
      midi_reference=60-3)
  )
  plot_affinity_brightness(dplyr::bind_rows(triad_of_triads),
                           'Rehding: Dualistic Brahms 114',
                           include_path=TRUE)
})
test_that('Rehding Monistic Forms',{
  triad_of_triads = list(
    h(c(-7,-4,0),root= -7,observation_point=TONIC,name='Expo 1',
      midi_reference=60+4),
    h(c(-4,0,3),root= -4,observation_point=TONIC,name='Expo 2',
      midi_reference=60+4),
    h(c(0,3,7),root= 0,observation_point=TONIC,name='Expo 3',
      midi_reference=60+4),
    h(c(0,3,7),root= 0,observation_point=TONIC,name='Coda 1',
      midi_reference=60-3),
    h(c(0,3,7),root= 0,observation_point=TONIC,name='Coda 2',
      midi_reference=60-3),
    h(c(0,4,7),root= 0,observation_point=TONIC,name='Coda 3',
      midi_reference=60-3)
  )
  plot_affinity_brightness(dplyr::bind_rows(triad_of_triads),
                           'Rehding: Monistic Brahms 114',
                           include_path=TRUE)
})
#
# Rehding stop
#
###############
#############
#
# Cohn start
#
test_that('Cohn figure 2.2 major-minor hexatonic_cycle',{
  hexatonic_cycle = list(
    h(c(-4, 0,3),root=-4,observation_point=TONIC ,name='Ab Major Root'),
    h(c(-4,-1,3),root=-4,observation_point=TONIC ,name='G# Minor Root'),
    h(c(-4,-1,4),root= 4,observation_point=OCTAVE,name='E Major 1st Inversion'),
    h(c(-5,-1,4),root= 4,observation_point=OCTAVE,name='E Minor 1st Inversion'),
    h(c(-5, 0,4),root= 0,observation_point=TONIC ,name='C Major 2nd Inversion'),
    h(c(-5, 0,3),root= 0,observation_point=TONIC ,name='C Minor 2nd Inversion'),
    h(c(-4, 0,3),root=-4,observation_point=TONIC ,include_label=FALSE)
  )
  plot_affinity_brightness(dplyr::bind_rows(hexatonic_cycle),
                           'Cohn: Major-Minor: Hexatonic Cycle',
                           include_path = TRUE)
})
test_that('major-phrygian hexatonic_cycle',{
  hexatonic_cycle = list(
    h(c(-4, 0,3),root=-4,observation_point=TONIC ,name='Ab Major Root'),
    h(c(-4,-1,3),root= 3,observation_point=OCTAVE,name='D# Phrygian Root'),
    h(c(-4,-1,4),root= 4,observation_point=OCTAVE,name='E Major 1st Inversion'),
    h(c(-5,-1,4),root=-1,observation_point=OCTAVE,name='B Phrygian 2nd Inversion'),
    h(c(-5, 0,4),root= 0,observation_point=TONIC ,name='C Major 2nd Inversion'),
    h(c(-5, 0,3),root=-5,observation_point=TONIC ,name='G Phrygian 1st Inversion'),
    h(c(-4, 0,3),root=-4,observation_point=TONIC ,include_label=FALSE)
  )
  plot_affinity_brightness(dplyr::bind_rows(hexatonic_cycle),
                           'Cohn: Major-Phrygian: Hexatonic Cycle',
                           include_path = TRUE)
})
test_that('lowest voice is root hexatonic_cycle',{
  hexatonic_cycle = list(
    h(c(-4, 0,3),root=-4,observation_point=TONIC,name='Ab Major Root'),
    h(c(-4,-1,3),root=-4,observation_point=TONIC,name='G# Minor Root'),
    h(c(-4,-1,4),root=-4,observation_point=TONIC,name='G# Locrian 1st Inversion'),
    h(c(-5,-1,4),root=-5,observation_point=TONIC,name='G Lydian Root'),
    h(c(-5, 0,4),root=-5,observation_point=TONIC,name='G Mixolydian 1st Inversion'),
    h(c(-5, 0,3),root=-5,observation_point=TONIC,name='G Phrygian 1st Inversion'),
    h(c(-4, 0,3),root=-4,observation_point=TONIC,include_label=FALSE)
  )
  plot_affinity_brightness(dplyr::bind_rows(hexatonic_cycle),
                           'Cohn: Lowest Voice is Root: Hexatonic Cycle',
                           include_path = TRUE)
})
test_that('highest voice is root hexatonic_cycle',{
  hexatonic_cycle = list(
    h(c(-4, 0,3),root=3,observation_point=OCTAVE,name='Eb Mixolydian Root'),
    h(c(-4,-1,3),root=3,observation_point=OCTAVE,name='D# Phrygian Root'),
    h(c(-4,-1,4),root=4,observation_point=OCTAVE,name='E Major 1st Inversion'),
    h(c(-5,-1,4),root=4,observation_point=OCTAVE,name='E Minor 1st Inversion'),
    h(c(-5, 0,4),root=4,observation_point=OCTAVE,name='E Locrian Root'),
    h(c(-5, 0,3),root=3,observation_point=OCTAVE,name='Eb Lydian 1st Inversion'),
    h(c(-4, 0,3),root=3,observation_point=OCTAVE,include_label=FALSE)
  )
  plot_affinity_brightness(dplyr::bind_rows(hexatonic_cycle),
                           'Cohn: Highest Voice is Root: Hexatonic Cycle',
                           include_path = TRUE)
})
test_that('Cohn figure 2.2 major-minor hexatonic_cycle',{
  hexatonic_cycle = list(
    h(c(-4, 0,3),root=-4,observation_point=TONIC ,name='Ab Major Root'),
    h(c(-4,-1,3),root=-4,observation_point=TONIC ,name='G# Minor Root'),
    h(c(-4,-1,4),root= 4,observation_point=OCTAVE,name='E Major 1st Inversion'),
    h(c(-5,-1,4),root= 4,observation_point=OCTAVE,name='E Minor 1st Inversion'),
    h(c(-5, 0,4),root= 0,observation_point=TONIC ,name='C Major 2nd Inversion'),
    h(c(-5, 0,3),root= 0,observation_point=TONIC ,name='C Minor 2nd Inversion'),
    h(c(-4, 0,3),root=-4,observation_point=TONIC ,include_label=FALSE)
  )
  plot_affinity_brightness(dplyr::bind_rows(hexatonic_cycle),
                           'Cohn: Major-Minor: Hexatonic Cycle',
                           include_path = TRUE)
})
#
# Cohn stop
#
############
################
#
# Tymoczko start
#
test_that('Tymoczko beholders eye 8.1a symmetry in traditional harmonic analysis',{
  progression = list(
    h(c(-12,0,4,7),root=-12,observation_point=TONIC,name='I  - C Major'),
    h(c(-7,0,5,9),root=-7,observation_point=TONIC,name='IV - F Major'),
    h(c(-5,-1,2,7),root=-5,observation_point=TONIC,name='V  - G Major'),
    h(c(-12,0,4,7),root=-12,observation_point=TONIC,name='I  - C Major')
  )
  plot_affinity_brightness(dplyr::bind_rows(progression),
                           "Tymoczko: 8.1a symmetry in traditional harmonic analysis",
                           include_path = TRUE)
})
test_that('Tymoczko: beholders eye 8.4a Inversion and diatonic transposition',{
  progression = list(
    h(c(-12,0,4,7),root=-12,observation_point=TONIC,name='I C Major'),
    h(c(-7,0,2,9),root=2,observation_point=OCTAVE,name='ii 6/5 D Minor 7'),
    h(c(-5,-1,2,7),root=-5,observation_point=TONIC,name='V G Major'),
    h(c(-12,0,4,7),root=-12,observation_point=TONIC,name='I C Major')
  )
  plot_affinity_brightness(dplyr::bind_rows(progression),
                           "Tymoczko: 8.4a Inversion and diatonic transposition",
                           include_path = TRUE)
})
test_that('Tymoczko: alt beholders eye 8.4a Inversion and diatonic transposition',{
  progression = list(
    h(c(-12,0,4,7),root=-12,observation_point=TONIC,name='I C Major'),
    h(c(-7,0,2,9),root=-7,observation_point=TONIC,name='IV F Major 7'),
    h(c(-5,-1,2,7),root=-5,observation_point=TONIC,name='V G Major'),
    h(c(-12,0,4,7),root=-12,observation_point=TONIC,name='I C Major')
  )
  plot_affinity_brightness(dplyr::bind_rows(progression),
                           "Tymoczko: 8.4a Alt Inversion and diatonic transposition",
                           include_path = TRUE)
})
test_that('Tristan Variations',{
  #  F, B, D♯, and G♯
  plot_affinity_brightness(chord_combinations(list(c(-7,-1,3,8))),
                           "Tristan Variations")
})
#
# Tymoczko stop
#
################
test_that('primary chord combinations 0 4 7',{
  plot_affinity_brightness(chord_combinations(list(c(0,4,7),-c(0,4,7) %>% sort)),
                           'Chord Combinations 047')
  expect_true(TRUE)
})
test_that('primary chord combinations 0 3 7',{
  plot_affinity_brightness(chord_combinations(list(c(0,3,7),-c(0,3,7) %>% sort)),
                           'Chord Combinations 037')
  expect_true(TRUE)
})
test_that('diatonic triads might be a surprise',{
  diatonic_triads = list(
    h(c(0,4,7)%>%sort,observation_point=TONIC,name='C Ionian P5/M3\nI-IV-V'),
    h(-c(0,4,7)%>%sort,midi_reference=60+4,observation_point=OCTAVE,name='E Phrygian m6/P4\nviii-v-iv'),
    h(-c(0,3,7)%>%sort,midi_reference=60-5,observation_point=OCTAVE,name='G Mixolydian M6/P4\nVIII-V-IV'),
    h(c(0,3,7)%>%sort,midi_reference=60-3,observation_point=TONIC,name='A Aeolian P5/m3\ni-iv-v'),
    h(-c(0,4,9)%>%sort,midi_reference=60-1,observation_point=OCTAVE,name='B Locrian m6/m3\nVIII-VII-IV'),
    h(c(0,4,9)%>%sort,midi_reference=60+5,observation_point=TONIC,name='F Lydian M6/M3\ni-ii-v'),
    h(c(0,2,7)%>%sort,midi_reference=60+2,observation_point=TONIC,name='D Dorian Up P5/M2\nI-III-IV-V-VII'),
    h(-c(0,2,7)%>%sort,midi_reference=60+2,observation_point=OCTAVE,name='D Dorian Down m7/P4\nviii-vi-v-iv-ii'),
    h(c(0,4,8)%>%sort,midi_reference=60+6,observation_point=TONIC,name='Augmented Triad Up m6/M3'),
    h(-c(0,4,8)%>%sort,midi_reference=60+6,observation_point=OCTAVE,name='Augmented Triad Down m6/M3')
  )
  plot_affinity_brightness(dplyr::bind_rows(diatonic_triads),
                           'Diatonic Triads')
})

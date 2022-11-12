plot_affinity_brightness <- function(chords,chords_name,include_path=FALSE) {
  dimensions = 'Affinity-Brightness'
  title = paste('Periodicity:',chords_name,dimensions)
  p=harmony_plot(chords,c('stolzenburg2015.brightness','stolzenburg2015.affinity'),
                 title=title,
                 include_path=include_path)
  save_harmony_plots(p)
  expect_true(!is.null(p))
  title = paste('Primes:',chords_name,dimensions)
  p=harmony_plot(chords,c('primes.brightness','primes.affinity'),
                 title=title,
                 include_path=include_path)
  save_harmony_plots(p)
  expect_true(!is.null(p))
  title = paste('Roughness:',chords_name,dimensions)
  p=harmony_plot(chords,c('hutchinson1978.brightness','hutchinson1978.affinity'),
                 title=title,
                 include_path=include_path)
  save_harmony_plots(p)
  expect_true(!is.null(p))
}
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
#
# Cohn stop
#
############
test_that('diatonic triads might be a surprise',{
  diatonic_triads = list(
    h(c(0,4,7)%>%sort,observation_point=TONIC,name='C Ionian P5/M3\nI-IV-V'),
    h((-c(0,4,7)+4)%>%sort,observation_point=OCTAVE,name='E Phrygian m6/P4\nviii-v-iv'),
    h((-c(0,3,7)-5)%>%sort,observation_point=OCTAVE,name='G Mixolydian M6/P4\nVIII-V-IV'),
    h((c(0,3,7)-3)%>%sort,observation_point=TONIC,name='A Aeolian P5/m3\ni-iv-v'),
    h((-c(0,4,9)-1)%>%sort,observation_point=OCTAVE,name='B Locrian M6/P4\nVIII-VII-IV'),
    h((c(0,4,9)+5)%>%sort,observation_point=TONIC,name='F Lydian M6/M3\ni-ii-v'),
    h((c(0,2,7)+2)%>%sort,observation_point=TONIC,name='D Dorian Up P5/M2\nI-III-IV-V-VII'),
    h((-c(0,2,7)+2)%>%sort,observation_point=OCTAVE,name='D Dorian Down m7/P4\nviii-vi-v-iv-ii'),
    h((c(0,4,8)+6)%>%sort,observation_point=TONIC,name='Augmented Triad Up m6/M3'),
    h((-c(0,4,8)+6)%>%sort,observation_point=OCTAVE,name='Augmented Triad Down m6/M3')
  )
  plot_affinity_brightness(dplyr::bind_rows(diatonic_triads),
                           'Diatonic Triads')
})
test_that('plot of affinity brighness of core pitches makes sense',{
  plot_affinity_brightness(core_pitches(),'Pitches')
})
test_that("plot major and minor triads", {
  plot_affinity_brightness(major_minor_triads(),'Major and Minor Triads')
})
test_that("plot triads in harmonic dualism", {
  plot_affinity_brightness(major_phrygian_triads(),'Harmonic Dualism? Major and Phrygian Triads')
})
test_that('diatonic modes look good',{
  chords = dplyr::bind_rows(diatonic_scales())
  plot_affinity_brightness(chords,'Diatonic Scales')
})

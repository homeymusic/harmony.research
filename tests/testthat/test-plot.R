plot_consonance_brightness <- function(chords,chords_name,include_path=FALSE) {
  dimensions = 'Brightness-Consonance'
  title = paste('Periodicity:',chords_name,dimensions)
  p=harmony_plot(chords,c('stolzenburg2015.brightness','stolzenburg2015.consonance'),
                 title=title,
                 include_path=include_path)
  save_harmony_plots(p)
  expect_true(!is.null(p))
  title = paste('Primes:',chords_name,dimensions)
  p=harmony_plot(chords,c('primes.brightness','primes.consonance'),
                 title=title,
                 include_path=include_path)
  save_harmony_plots(p)
  expect_true(!is.null(p))
  title = paste('Roughness:',chords_name,dimensions)
  p=harmony_plot(chords,c('hutchinson1978.brightness','hutchinson1978.consonance'),
                 title=title,
                 include_path=include_path)
  save_harmony_plots(p)
  expect_true(!is.null(p))
}
test_that('plot of consonance brighness of core pitches makes sense',{
  plot_consonance_brightness(core_pitches(),'Pitches')
})
test_that("plot major and minor triads", {
  plot_consonance_brightness(major_minor_triads(),'Major and Minor Triads')
})
test_that("plot triads in harmonic dualism", {
  plot_consonance_brightness(major_phrygian_triads(),'Major and Dual Minor Triads')
})
test_that('diatonic modes look good',{
  chords = dplyr::bind_rows(diatonic_scales())
  plot_consonance_brightness(chords,'Diatonic Scales')
})
test_that('diatonic traids look good',{
  chords = dplyr::bind_rows(diatonic_triads())
  plot_consonance_brightness(chords,'Diatonic Triads')
})
test_that('the value of root and observation point make sense with major triad',{
  chords = list(
    h(c(0,4,7),  root=0, observation_point=TONIC, name="C Major"),
    h(c(0,4,7),  root=0, observation_point=OCTAVE, name="C -P5/-M3 (Imaginary?)"),
    h(c(4,7,12), root=4, observation_point=TONIC, name="E m6/m3"),
    h(c(4,7,12), root=12,observation_point=OCTAVE,name='C 1st Inversion'),
    h(c(7,12,16),root=7, observation_point=TONIC, name="G M6/P4"),
    h(c(7,12,16),root=12, observation_point=TONIC, name="C 2nd Inversion"),
    h(c(7,12,16),root=12, observation_point=OCTAVE, name="C -P4/+M3 (Inverted M6/P4?)"),
    h(c(4,7,12), root=12,observation_point=TONIC, name="C -m6/-P4 (Imaginary?)")
  )
  plot_consonance_brightness(dplyr::bind_rows(chords),
                           'Root Direction Mechanics')
})

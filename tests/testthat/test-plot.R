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
test_that("plot core pitches", {
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
    h(c(0,3)  ,observation_point=0),
    h(c(0,8)  ,observation_point=0),
    h(c(0,5)  ,observation_point=0))
  plot_affinity_brightness(chords,'Major 1st Inversion')
})

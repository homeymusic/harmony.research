test_that("plot core pitches", {
  title='Pitches Affinity v Brightness stolzenburg2015'
  p=harmony_plot(core_pitches(),c('stolzenburg2015.brightness','stolzenburg2015.affinity'),title=title)
  save_harmony_plots(p)
  expect_true(!is.null(p))

  title='Pitches Affinity v Brightness primes'
  p=harmony_plot(core_pitches(),c('primes.brightness','primes.affinity'),title=title)
  save_harmony_plots(p)
  expect_true(!is.null(p))
})
test_that("plot all tonic dyads", {
  combos = utils::combn(1:11,1,function(x){c(0,x)},simplify=FALSE)

  chords_up   = combos %>% purrr::map(h,direction=+1)
  chords_down = combos %>% purrr::map(h,direction=-1)
  chords = dplyr::bind_rows(dplyr::bind_rows(chords_up),dplyr::bind_rows(chords_down))

  title='Tonic Dyads Affinity v Brightness stolzenburg2015'
  p=harmony_plot(chords,c('stolzenburg2015.brightness','stolzenburg2015.affinity'),title=title)
  save_harmony_plots(p)
  expect_true(!is.null(p))

  title='Tonic Dyads Affinity v Brightness primes'
  p=harmony_plot(chords,c('primes.brightness','primes.affinity'),title=title)
  save_harmony_plots(p)
  expect_true(!is.null(p))
})
test_that("plot all octave dyads", {
  combos = utils::combn(1:11,1,function(x){c(x,12)},simplify=FALSE)

  chords_up   = combos %>% purrr::map(h,direction=+1)
  chords_down = combos %>% purrr::map(h,direction=-1)
  chords = dplyr::bind_rows(dplyr::bind_rows(chords_up),dplyr::bind_rows(chords_down))

  title='Octave Dyads Affinity v Brightness stolzenburg2015'
  p=harmony_plot(chords,c('stolzenburg2015.brightness','stolzenburg2015.affinity'),title=title)
  save_harmony_plots(p)
  expect_true(!is.null(p))

  title='Octave Dyads Affinity v Brightness primes'
  p=harmony_plot(chords,c('primes.brightness','primes.affinity'),title=title)
  save_harmony_plots(p)
  expect_true(!is.null(p))
})
test_that("plot all 3-note tonic chords", {
  combos = utils::combn(1:11,2,function(x){c(0,x)},simplify=FALSE)

  chords_up   = combos %>% purrr::map(h,direction=+1)
  chords_down = combos %>% purrr::map(h,direction=-1)
  chords = dplyr::bind_rows(dplyr::bind_rows(chords_up),dplyr::bind_rows(chords_down))

  title='3-Note Tonic Chords Affinity v Brightness stolzenburg2015'
  p=harmony_plot(chords,c('stolzenburg2015.brightness','stolzenburg2015.affinity'),title=title)
  save_harmony_plots(p)
  expect_true(!is.null(p))

  title='3-Note Tonic Chords Affinity v Brightness primes'
  p=harmony_plot(chords,c('primes.brightness','primes.affinity'),title=title)
  save_harmony_plots(p)
  expect_true(!is.null(p))
})

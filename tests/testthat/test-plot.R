test_that("plot core pitches", {
  title='Periodicty: Pitches Affinity v Brightness'
  p=harmony_plot(core_pitches(),c('stolzenburg2015.brightness','stolzenburg2015.affinity'),title=title)
  save_harmony_plots(p)
  expect_true(!is.null(p))

  title='Primes: Pitches Affinity v Brightness'
  p=harmony_plot(core_pitches(),c('primes.brightness','primes.affinity'),title=title)
  save_harmony_plots(p)
  expect_true(!is.null(p))
})
test_that("plot all dyads", {
  combos = utils::combn(1:11,1,function(x){c(0,x)},simplify=FALSE)
  inverted_combos = utils::combn(1:11,1,function(x){c(x,12)},simplify=FALSE)

  chords_up            = combos %>% purrr::map(h,direction=+1)
  inverted_chords_down = inverted_combos %>% purrr::map(h,direction=-1)
  chords = dplyr::bind_rows(
    dplyr::bind_rows(chords_up),dplyr::bind_rows(inverted_chords_down))

  title='Periodicty: Dyads Affinity v Brightness'
  p=harmony_plot(chords,c('stolzenburg2015.brightness','stolzenburg2015.affinity'),title=title)
  save_harmony_plots(p)
  expect_true(!is.null(p))

  title='Primes: Dyads Affinity v Brightness'
  p=harmony_plot(chords,c('primes.brightness','primes.affinity'),title=title)
  save_harmony_plots(p)
  expect_true(!is.null(p))
})
test_that("plot all triads", {
  combos = utils::combn(1:11,2,function(x){c(0,x)},simplify=FALSE)
  inverted_combos = utils::combn(1:11,2,function(x){c(x,12)},simplify=FALSE)

  chords_up            =  combos %>% purrr::map(h,direction=+1)
  inverted_chords_down =  inverted_combos %>% purrr::map(h,direction=-1)
  chords = dplyr::bind_rows(
    dplyr::bind_rows(chords_up),dplyr::bind_rows(inverted_chords_down))

  title='Periodicty: Triads Affinity v Brightness'
  p=harmony_plot(chords,c('stolzenburg2015.brightness','stolzenburg2015.affinity'),title=title)
  save_harmony_plots(p)
  expect_true(!is.null(p))

  title='Primes: Triads Affinity v Brightness'
  p=harmony_plot(chords,c('primes.brightness','primes.affinity'),title=title)
  save_harmony_plots(p)
  expect_true(!is.null(p))
})
test_that("plot compound dyads", {
  combos = utils::combn(1:11,1,function(x){c(-12,x)},simplify=FALSE)
  inverted_combos = utils::combn(1:11,1,function(x){c(x,24)},simplify=FALSE)

  chords_up            =  combos %>% purrr::map(h,direction=+1)
  inverted_chords_up   =  inverted_combos %>% purrr::map(h,direction=+1)
  chords_down          =  combos %>% purrr::map(h,direction=-1)
  inverted_chords_down =  inverted_combos %>% purrr::map(h,direction=-1)
  chords = dplyr::bind_rows(
    dplyr::bind_rows(chords_up),dplyr::bind_rows(inverted_chords_up),
    dplyr::bind_rows(chords_down),dplyr::bind_rows(inverted_chords_down)
  )

  title='Periodicty: Dyads Compound Affinity v Brightness'
  p=harmony_plot(chords,c('stolzenburg2015.brightness','stolzenburg2015.affinity'),title=title)
  save_harmony_plots(p)
  expect_true(!is.null(p))

  title='Primes: Dyads Compound Affinity v Brightness'
  p=harmony_plot(chords,c('primes.brightness','primes.affinity'),title=title)
  save_harmony_plots(p)
  expect_true(!is.null(p))
})
test_that("plot compound triads", {
  combos = utils::combn(1:11,2,function(x){c(-12,x)},simplify=FALSE)
  inverted_combos = utils::combn(1:11,2,function(x){c(x,24)},simplify=FALSE)

  chords_up            =  combos %>% purrr::map(h,direction=+1)
  inverted_chords_up   =  inverted_combos %>% purrr::map(h,direction=+1)
  chords_down          =  combos %>% purrr::map(h,direction=-1)
  inverted_chords_down =  inverted_combos %>% purrr::map(h,direction=-1)
  chords = dplyr::bind_rows(
    dplyr::bind_rows(chords_up),dplyr::bind_rows(inverted_chords_up),
    dplyr::bind_rows(chords_down),dplyr::bind_rows(inverted_chords_down)
  )

  title='Periodicty: Triads Compound Affinity v Brightness'
  p=harmony_plot(chords,c('stolzenburg2015.brightness','stolzenburg2015.affinity'),title=title)
  save_harmony_plots(p)
  expect_true(!is.null(p))

  title='Primes: Triads Compound Affinity v Brightness'
  p=harmony_plot(chords,c('primes.brightness','primes.affinity'),title=title)
  save_harmony_plots(p)
  expect_true(!is.null(p))
})

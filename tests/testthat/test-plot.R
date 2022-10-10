test_that("plot all 3-note chords", {
  combos = utils::combn(1:11,2,function(x){c(0,x)},simplify=FALSE)

  chords_up   = combos %>% purrr::map(h)
  chords_down = combos %>% purrr::map(h,direction=-1)
  chords = dplyr::bind_rows(dplyr::bind_rows(chords_up),dplyr::bind_rows(chords_down))

  title='3-Note Chords Affinity v Brightness'
  p=harmony_plot(chords,c('stolzenburg2015.brightness','stolzenburg2015.affinity'),title=title)
  save_harmony_plots(p)
  expect_true(!is.null(p))
})

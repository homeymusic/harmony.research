consonance.stolzenburg2015.uncached <- function(chord) {
  checkmate::assert_integerish(chord)

  tonic.dissonance  = smoothed_relative_periodicity(chord,'tonic')
  octave.dissonance = smoothed_relative_periodicity(chord,'octave')

  ###################################################################################
  # this is the 'heavy lifting' for calculating affinity, brightness and consonance
  #
  # calculate 2-dimensional tonic-octave dissonance
  tonic_octave_dissonance = cbind(tonic.dissonance,octave.dissonance)
  # flip orientation to 2-dimensional tonic-octave consonance
  tonic_octave_consonance = consonance.stolzenburg2015.max_dissonance() - tonic_octave_dissonance
  # rotate pi/4 (45 deg) to 2-dimensional affinity-brightness

  print(paste('tonic_octave_consonance',tonic_octave_consonance))

  affinity_brightness = tonic_octave_consonance %>% rotate(pi/4)

  # store the ABCDs: affinity brightness consonance dissonance
  t = tibble::tibble_row(
    tonic.dissonance  = tonic_octave_dissonance[1,1],
    octave.dissonance = tonic_octave_dissonance[1,2],
    tonic.consonance  = tonic_octave_consonance[1,1],
    octave.consonance = tonic_octave_consonance[1,2],
    affinity          = affinity_brightness[1,2],
    brightness        = affinity_brightness[1,1]
  )
  t
}

#' Consonance:
#'
#' Implements Stolzenburg's 2015 periodicity metric of consonance
#'
#' @param chord A chord expressed as an interval integers
#' @return A tibble
#'
#' @export
consonance.stolzenburg2015 <- memoise::memoise(consonance.stolzenburg2015.uncached)

smoothed_relative_periodicity <- function(x,dimension) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(dimension,c('tonic','octave'))

  seq_along(x) %>%
    purrr::map_dbl(~log2(relative_periodicity(x-x[.x],dimension))) %>%
    mean
}

relative_periodicity <- function(x,dimension) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(dimension,c('tonic','octave'))

  pitches = dplyr::bind_rows(x %>% sort %>% purrr::map(pitch))
  lcm(pitches[[paste0(dimension,'.ref')]]) *
    pitches[[paste0(dimension,'.pitch')]][1] /
    pitches[[paste0(dimension,'.ref')]][1]
}

lcm <- function(x) {
  if (length(x) == 1L) x else if (length(x) == 2L) {
    gmp::lcm.default(x[1], x[2])
  } else lcm(c(x[1], lcm(x[-1])))
}

# we are using the semitone, the minor second m2 from the tonic as max dissonance
# the result would be the same if we used major 7th M7 from octave perspective
consonance.stolzenburg2015.max_dissonance <- function() {
  smoothed_relative_periodicity(c(0,1),'tonic')
}

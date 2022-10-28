consonance.stolzenburg2015.uncached <- function(chord) {
  checkmate::assert_integerish(chord)

  ###################################################################################
  # this is the 'heavy lifting' for calculating affinity, brightness and consonance
  #
  tonic  = relative_periodicity(chord,observation_point=0)
  octave = relative_periodicity(chord,observation_point=12)
  # calculate 2-dimensional tonic-octave dissonance
  tonic_octave_dissonance = cbind(tonic,octave)
  # flip orientation to 2-dimensional tonic-octave consonance
  tonic_octave_consonance = consonance.stolzenburg2015.max_dissonance() - tonic_octave_dissonance
  # rotate pi/4 (45 deg) to 2-dimensional affinity-brightness
  affinity_brightness = tonic_octave_consonance %>% rotate(pi/4)

  # store the ABCDs: affinity brightness consonance dissonance
  tibble::tibble_row(
    tonic.dissonance  = tonic_octave_dissonance[1,1],
    octave.dissonance = tonic_octave_dissonance[1,2],
    tonic.consonance  = tonic_octave_consonance[1,1],
    octave.consonance = tonic_octave_consonance[1,2],
    affinity          = affinity_brightness[1,2],
    brightness        = affinity_brightness[1,1]
  )
}

#' Consonance:
#'
#' Implements Stolzenburg's 2015 periodicity metric of consonance
#'
#' @param chord A chord expressed as a vector of integers
#' @return A tibble
#'
#' @export
consonance.stolzenburg2015 <- memoise::memoise(consonance.stolzenburg2015.uncached)

relative_periodicity <- function(x,observation_point) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(observation_point,c(0,12))

  pitches = dplyr::bind_rows(x %>% purrr::map(pitch))
  log2(lcm(if (observation_point==0) pitches$tonic.den.lo else pitches$octave.num.lo))
}

# from https://github.com/pmcharrison/stolz15
lcm <- function(x) {
  if (length(x) == 1L) x else if (length(x) == 2L) {
    gmp::lcm.default(x[1], x[2])
  } else lcm(c(x[1], lcm(x[-1])))
}

consonance.stolzenburg2015.max_dissonance.uncached <- function() {
  # this is arbitrary: using the minor 2nd
  relative_periodicity(c(0,1),observation_point=0)
}
consonance.stolzenburg2015.max_dissonance <- memoise::memoise(consonance.stolzenburg2015.max_dissonance.uncached)

consonance.stolzenburg2015.uncached <- function(chord) {
  checkmate::assert_integerish(chord)

  tonic  = relative_periodicity(chord,'tonic')
  octave = relative_periodicity(chord,'octave')

  ###################################################################################
  # this is the 'heavy lifting' for calculating affinity, brightness and consonance
  #
  # calculate 2-dimensional tonic-octave dissonance
  tonic_octave_dissonance = cbind(tonic,octave)
  # flip orientation to 2-dimensional tonic-octave consonance
  tonic_octave_consonance = consonance.stolzenburg2015.max_dissonance() - tonic_octave_dissonance
  # rotate pi/4 (45 deg) to 2-dimensional affinity-brightness
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
#' @param chord A chord expressed as a vector of integers
#' @return A tibble
#'
#' @export
consonance.stolzenburg2015 <- memoise::memoise(consonance.stolzenburg2015.uncached)

relative_periodicity <- function(x,observation_point) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(observation_point,c('tonic','octave'))
  lowest_period_length <- ratios_lower_pitches <- minimum_ratio <- NULL

  pitches = dplyr::bind_rows(x %>% sort %>% purrr::map(pitch))
  if (observation_point          == 'tonic') {
    lowest_period_length = pitches$tonic.den.lo[1]  / pitches$tonic.num.hi[1]
    lowest_pitches       = pitches$tonic.den.lo
    minimum_ratio        = pitches$tonic.num.hi[1]  / pitches$tonic.den.lo[1]
  } else if (observation_point   == 'octave') {
    lowest_period_length = pitches$octave.den.hi[1] / pitches$octave.num.lo[1]
    lowest_pitches       = pitches$octave.num.lo
    minimum_ratio        = pitches$octave.num.lo[1] / pitches$octave.den.hi[1]
  }
  log2(lowest_period_length * lcm(lowest_pitches) * minimum_ratio)
}

lcm <- function(x) {
  if (length(x) == 1L) x else if (length(x) == 2L) {
    gmp::lcm.default(x[1], x[2])
  } else lcm(c(x[1], lcm(x[-1])))
}

consonance.stolzenburg2015.max_dissonance <- function() {
  # this is completely arbitrary
  # using the minor 2nd logarithmically
  # it does turn out to be exactly 15, like the max from the primes measure
  2^(relative_periodicity(c(0,1),'tonic'))
}

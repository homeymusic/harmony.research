consonance.stolzenburg2015.uncached <- function(chord) {
  checkmate::assert_integerish(chord)

  #############################################################################
  # calculate affinity, brightness, consonance and dissonance
  #

  # calculate dissonance from the tonic and octave observation points
  tonic_dissonance  = relative_periodicity(chord,observation_point=TONIC)
  octave_dissonance = relative_periodicity(chord,observation_point=OCTAVE)
  # create 2-dimensional tonic-octave dissonance matrix
  tonic_octave_dissonance = cbind(tonic_dissonance,octave_dissonance)
  # reverse the orientation to tonic-octave consonance matrix
  tonic_octave_consonance = consonance.stolzenburg2015.max_dissonance() -
    tonic_octave_dissonance
  # rotate tonic-octave consonance matrix by pi/4 to affinity-brightness matrix
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
consonance.stolzenburg2015 <-
  memoise::memoise(consonance.stolzenburg2015.uncached)

relative_periodicity <- function(x,observation_point) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(observation_point,c(TONIC,OCTAVE))

  # create pitch objects for all pitches in the chord
  # each pitch object includes the frequency ratio from the tonic perspective
  # and the frequency ratio from the octave perspective (see pitch.R)
  pitches = dplyr::bind_rows(x %>% purrr::map(pitch))
  # calculate the relative periodicity
  log2(lcm(
    if (observation_point==TONIC)
      # from the tonic perspective the lower pitch is in the ratio denominator
      pitches$tonic.den.lo
    else
      # from the octave perspective the lower pitch is in the ratio numerator
      pitches$octave.num.lo
  ))
}

# from https://github.com/pmcharrison/stolz15
lcm <- function(x) {
  if (length(x) == 1L) x else if (length(x) == 2L) {
    gmp::lcm.default(x[1], x[2])
  } else lcm(c(x[1], lcm(x[-1])))
}

consonance.stolzenburg2015.max_dissonance.uncached <- function() {
  # this is arbitrary: using the chromatic chord
  relative_periodicity(TONIC:OCTAVE,observation_point=TONIC)
}
consonance.stolzenburg2015.max_dissonance <-
  memoise::memoise(consonance.stolzenburg2015.max_dissonance.uncached)

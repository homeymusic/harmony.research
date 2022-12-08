consonance.stolzenburg2015.uncached <- function(chord) {
  checkmate::assert_integerish(chord)

  #############################################################################
  # calculate brightness, consonance and dissonance
  #

  # calculate dissonance from the tonic and octave observation points
  tonic_dissonance  = relative_periodicity(chord,observation_point=TONIC)
  octave_dissonance = relative_periodicity(chord,observation_point=OCTAVE)
  # create 2-dimensional tonic-octave dissonance matrix
  tonic_octave_dissonance = cbind(tonic_dissonance,octave_dissonance)
  # using max dissonance, reverse orientation from dissonance to consonance
  tonic_octave_consonance = stolzenburg2015.max_dissonance() -
    tonic_octave_dissonance
  # rotate tonic-octave consonance matrix by pi/4 to consonance-brightness matrix
  consonance_brightness = tonic_octave_consonance %>% rotate(pi/4)

  # store the brightness consonance and dissonance
  tibble::tibble_row(
    tonic.dissonance  = tonic_octave_dissonance[1,1],
    octave.dissonance = tonic_octave_dissonance[1,2],
    tonic.consonance  = tonic_octave_consonance[1,1],
    octave.consonance = tonic_octave_consonance[1,2],
    consonance        = consonance_brightness[1,2],
    brightness        = consonance_brightness[1,1]
  )
}

#' Consonance:
#'
#' Implements a tonic-octave variation of Stolzenburg's 2015 tonic-only
#' consonance periodicity metric
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
      # from tonic perspective, lower pitch is represented by ratio denominator
      # (see pitch.R)
      pitches$tonic.den.lo
    else
      # from octave perspective, lower pitch is represented by ratio numerator
      # (see pitch.R)
      pitches$octave.num.lo
  ))
}

# from https://github.com/pmcharrison/stolz15
lcm <- function(x) {
  if (length(x) == 1L) x else if (length(x) == 2L) {
    gmp::lcm.default(x[1], x[2])
  } else lcm(c(x[1], lcm(x[-1])))
}

stolzenburg2015.max_dissonance.uncached <- function() {
  # this is arbitrary: sit on the piano
  # using the chromatic chord for max dissonance
  relative_periodicity(TONIC:OCTAVE,observation_point=TONIC)
}
stolzenburg2015.max_dissonance <-
  memoise::memoise(stolzenburg2015.max_dissonance.uncached)

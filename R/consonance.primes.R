consonance.primes.uncached <- function(chord) {
  checkmate::assert_integerish(chord)

  pitches = chord %>% purrr::map(~pitch(.x))

  tonic.dissonance  = pitches %>% purrr::map_dbl(
    ~prime_factors_sum(.x$tonic.num.hi, .x$tonic.den.lo))   %>% mean

  octave.dissonance = pitches %>% purrr::map_dbl(
    ~prime_factors_sum(.x$octave.num.lo, .x$octave.den.hi)) %>% mean

  ###################################################################################
  # this is the 'heavy lifting' for calculating affinity, brightness and consonance
  #
  # calculate 2-dimensional tonic-octave dissonance
  tonic_octave_dissonance = cbind(tonic.dissonance,octave.dissonance)
  # flip orientation to 2-dimensional tonic-octave consonance
  tonic_octave_consonance = consonance.primes.max_dissonance() - tonic_octave_dissonance
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

#' Consonance: Prime Factors Sum
#'
#' Provides a simple metric of consonance based on the sum of prime factors
#'
#'
#' @param chord A chord expressed as an interval integers
#' @return A tibble
#'
#' @export
consonance.primes <- memoise::memoise(consonance.primes.uncached)

prime_factors_sum <- function(numerator,denominator) {
  checkmate::assert_integerish(numerator,denominator)

  ratio = c(numerator,denominator)

  ratio[ratio>1] %>% purrr::map(~numbers::primeFactors(.x)) %>% unlist %>% sum
}

# we are using the semitone, the minor second m2 from the tonic as max dissonance
# the result would be the same if we used major 7th M7 from octave perspective
consonance.primes.max_dissonance <- function() {
  m2 = pitch(1) # minor 2nd
  prime_factors_sum(m2$tonic.num.hi, m2$tonic.den.lo)
}

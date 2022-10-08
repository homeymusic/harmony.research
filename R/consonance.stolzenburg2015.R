consonance.stolzenburg2015.uncached <- function(chord) {
  checkmate::assert_integerish(chord)

  tonic.dissonance  = log_relative_periodicity(chord,'tonic')
  octave.dissonance = log_relative_periodicity(chord,'octave')

  ###################################################################################
  # this is the 'heavy lifting' for calculating affinity, brightness and consonance
  #
  # calculate 2-dimensional tonic-octave dissonance
  tonic_octave_dissonance = cbind(tonic.dissonance,octave.dissonance)
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
#' @param chord A chord expressed as an interval integers
#' @return A tibble
#'
#' @export
consonance.stolzenburg2015 <- memoise::memoise(consonance.stolzenburg2015.uncached)

log_relative_periodicity <- function(x,dimension) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(dimension,c('tonic','octave'))
  if (dimension == 'tonic') {
    pitches = dplyr::bind_rows(x %>% sort %>% purrr::map(pitch))
    log2(lcm(pitches$tonic.ref) *
           pitches$tonic.pitch[1] / pitches$tonic.ref[1])
  } else if (dimension == 'octave') {
    pitches = dplyr::bind_rows(x %>% sort %>% purrr::map(pitch))
    log2(lcm(pitches$octave.pitch) *
           pitches$octave.ref[1] / pitches$octave.pitch[1] / 2)
  }
}

lcm <- function(x) {
  if (length(x) == 1L) x else if (length(x) == 2L) {
    gmp::lcm.default(x[1], x[2])
  } else lcm(c(x[1], lcm(x[-1])))
}

# we are using the semitone, the minor second m2 from the tonic as max dissonance
# the result would be the same if we used major 7th M7 from octave perspective
consonance.stolzenburg2015.max_dissonance <- function() {
  log_relative_periodicity(c(0,1),'tonic')
  10
}
